{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ArgumentsMain (Output (..), HasExtension (..), OutputFormat (..), Input (..), Arguments (Arguments), arguments, StrategyType (..), InputType (..))
import Compiler.CoreIR (prettyCoreProgram)
import Compiler.CoreIRToForSyDeIR (translateCoreProgram)
import Compiler.ForSyDeIR (prettyIRJSON, prettyIRSystem, IRSystem)
import Compiler.ForSyDeIRToProceduralIR (generateInputHeader, translateIRSystemToProgram)
import Compiler.GHCCoreAPI (compileToCoreWithForSyDePath)
import Compiler.ProceduralIR (prettyProgram)
import Compiler.ProceduralIRToC (translateProgram)
import Compiler.SchedulerMain (generateSchedule, prettySchedule)
import Compiler.Scheduler.Common (SchedulingStrategy (..), Schedule (..))
import Options.Applicative
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath ((</>))
import System.Directory
  ( createDirectoryIfMissing,
    createDirectoryLink,
    doesFileExist,
    doesPathExist,
    getCurrentDirectory,
    makeAbsolute,
    pathIsSymbolicLink,
  )
import System.Environment (lookupEnv)
import System.FilePath
  ( dropExtension,
    joinPath,
    normalise,
    splitDirectories,
    takeBaseName,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import Control.Exception (IOException, catch)
import Control.Monad (unless, when)

{-

    Specification

    Compiler steps:

    ForSyDe model ->
    Core ->
    ForSyDe IR ->
    Procedural IR ->
    C

    - First we need a source file.
    - Input mode:
        - This should be the format for the input file, if not otherwise
          specified, this should be a ForSyDe model (.hs) file

    - Output mode:
        - If --output-c, output file should be in C
        - If --output-core, output should be in core format
        - If --output-forsyde-ir, output should be in ForSyDe-IR format
        - If --output-forsyde-ir-json, output should be in ForSyDe-IR-JSON
          format
        - If --output-procedural-ir, output should be in procedural IR format

    - Output file argument:
        - Use "-o" flag, outputs to desired file
        - Dump to a file "main.xxx" where file format depends on output format
          flag if unspecified.
        - If "--stdout", dump to stdout.

-}

write_output :: Output -> OutputFormat -> [Char] -> IO ()
write_output StdOut _ s =
  putStr s
write_output (OutputFile (output_file, WithFileExtension)) _ s =
  writeFile output_file s
write_output (OutputFile (output_file, NoFileExtension)) extension s =
  writeFile (generateDefaultFileName output_file extension) s

-- Function for generating default file names
generateDefaultFileName :: FilePath -> OutputFormat -> FilePath
generateDefaultFileName f OutputC = f ++ ".c"
generateDefaultFileName f OutputCore = f ++ ".hcr"
generateDefaultFileName f OutputForSyDeIR = f ++ ".fir"
generateDefaultFileName f OutputForSyDeIRJSON = f ++ ".json"
generateDefaultFileName f OutputProceduralIR = f ++ ".pir"
generateDefaultFileName f OutputSchedule = f ++ ".sched"

-- Main function for running after arguments have been returned from main.
-- Pattern matches on the requested output type to determine which stages
-- of the compilation pipeline are required.
-- They are matched on manually since a lot of combinations are invalid.
-- You can't for intance start with procedural IR as input and go to Core
-- since the compiler doesn't go backwards.

run :: Arguments -> IO ()
run (Arguments (InputFile input_file) output_file outputType target inputType runs pkgPath stratOpt numCores) = do
  (core, dflags) <- compileToCoreWithForSyDePath pkgPath input_file
  let (outputIR, iomaybe) = case outputType of
        OutputCore ->
          (prettyCoreProgram dflags core, Nothing)
        OutputForSyDeIR -> do
          let (forsydeIR, _) = translateCoreHelper dflags core
          (prettyIRSystem dflags forsydeIR, Nothing)
        OutputForSyDeIRJSON -> do
          let (forsydeIR, _) = translateCoreHelper dflags core
          (prettyIRJSON forsydeIR, Nothing)
        OutputSchedule -> do
          let (forsydeIR, _) = translateCoreHelper dflags core
          let sched = computeScheduleHelper forsydeIR
          (prettySchedule forsydeIR sched, Nothing)
        OutputProceduralIR -> do
          let (forsydeIR, lookupSignals) = translateCoreHelper dflags core
          let sched = computeScheduleHelper forsydeIR
          (prettyProgram $ translateIRSystemToProgram dflags sched lookupSignals target inputType runs forsydeIR, Nothing)
        OutputC -> do
          let (forsydeIR, lookupSignals) = translateCoreHelper dflags core
          let sched = computeScheduleHelper forsydeIR
          let isMultiCore = length (schedPerCore sched) > 1
          let effectiveInputType = if isMultiCore then Predefined else inputType
          let proceduralIR = translateIRSystemToProgram dflags sched lookupSignals target effectiveInputType runs forsydeIR
          let outputString = translateProgram proceduralIR target effectiveInputType True
          let io = case output_file of
                StdOut -> do
                  putStr outputString
                _ -> do
                  cFilePath <- setupCOutput input_file output_file effectiveInputType forsydeIR sched
                  writeFile cFilePath outputString
          (outputString, Just io)
  case iomaybe of
    Nothing -> 
      write_output output_file outputType outputIR
    Just io ->
      io
  where
    translateCoreHelper dflags core = case translateCoreProgram dflags core of
      Left e -> error e
      Right v -> v
    computeScheduleHelper forsydeIR = case generateSchedule strat forsydeIR of
      Left e -> error e
      Right sched -> sched
    strat = case stratOpt of
        StrategyGreedy -> Greedy
        StrategyHuLevel -> HuLevel numCores

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (arguments <**> helper)
        ( fullDesc
            <> progDesc "Compile a ForSyDe model"
            <> header "ForSyDe DevTools"
        )

setupCOutput :: FilePath -> Output -> InputType -> IRSystem -> Schedule -> IO FilePath
setupCOutput inputPath outputVal effectiveInputType forsydeIR schedule = do
  let (targetDir, cFilePath) = resolveCOutput inputPath outputVal
  createDirectoryIfMissing True targetDir
  when (effectiveInputType == Predefined) $ do
    let inputHeaderPath = targetDir </> "input.h"
    headerExists <- doesFileExist inputHeaderPath
    unless headerExists $
      writeFile inputHeaderPath (generateInputHeader forsydeIR schedule)
  setupIncludeSymlink targetDir
  pure cFilePath

-- | Resolves the target output directory and C source file path based on the input model
-- and the requested output path.
-- - If -o specifies a directory (or has no .c extension), puts main.c inside that directory.
-- - If -o specifies a file with a parent directory (e.g. -o dir/test.c), keeps that file and uses dir.
-- - If -o specifies a simple filename (e.g. -o test.c), creates a folder named after the file stem (e.g. test/test.c).
-- - If default -o main is used, creates a folder named after the model stem (e.g. SDF_example_002/main.c).
resolveCOutput :: FilePath -> Output -> (FilePath, FilePath)
resolveCOutput inputPath outputVal = case outputVal of
  StdOut -> ("", "")
  OutputFile (rawPath, hasExt) ->
    let ext = takeExtension rawPath
        dir = takeDirectory rawPath
        stem = takeBaseName inputPath
     in if ext == ".c"
          then
            if dir == "" || dir == "."
              then let outDir = "output" </> dropExtension (takeFileName rawPath) in (outDir, outDir </> takeFileName rawPath)
              else (dir, "output" </> rawPath)
          else
            let targetDir = case (rawPath, hasExt) of
                  ("main", NoFileExtension) -> "output" </> stem
                  _ -> "output" </> rawPath
             in (targetDir, targetDir </> "main.c")

-- | Sets up a relative symbolic link from `targetDir/include` to the repository's `include` directory.
setupIncludeSymlink :: FilePath -> IO ()
setupIncludeSymlink targetDir = do
  mIncDir <- findIncludeDir
  case mIncDir of
    Nothing -> pure ()
    Just incDir -> do
      absTargetDir <- makeAbsolute targetDir
      absIncDir <- makeAbsolute incDir
      let targetInclude = absTargetDir </> "include"
      -- Don't create symlink if targetDir already IS or CONTAINS the real include directory
      unless (normalise targetInclude == normalise absIncDir || normalise absTargetDir == normalise absIncDir) $ do
        linkExists <- doesPathExist targetInclude
        isSymlink <- pathIsSymbolicLink targetInclude `catch` (\(_ :: IOException) -> pure False)
        unless (linkExists || isSymlink) $ do
          let relIncDir = computeRelativePath absTargetDir absIncDir
          createDirectoryLink relIncDir targetInclude

-- | Searches for the `include` directory containing `common.h` in the environment,
-- current directory, or any parent directory.
findIncludeDir :: IO (Maybe FilePath)
findIncludeDir = do
  mEnv <- lookupEnv "FORSYDE_INCLUDE_PATH"
  case mEnv of
    Just p -> do
      ex <- doesFileExist (p </> "common.h")
      if ex then pure (Just p) else searchParents
    Nothing -> searchParents
  where
    searchParents = do
      cwd <- getCurrentDirectory
      search cwd
    search dir = do
      let cand = dir </> "include"
      ex <- doesFileExist (cand </> "common.h")
      if ex
        then pure (Just cand)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure Nothing
            else search parent

-- | Computes a relative path from directory 'from' to directory 'to'.
computeRelativePath :: FilePath -> FilePath -> FilePath
computeRelativePath from to =
  let fromDirs = splitDirectories from
      toDirs = splitDirectories to
      (_, fromRest, toRest) = stripCommon fromDirs toDirs
      upDirs = replicate (length fromRest) ".."
   in if null upDirs && null toRest
        then "."
        else joinPath (upDirs ++ toRest)
  where
    stripCommon (x : xs) (y : ys)
      | x == y = let (c, r1, r2) = stripCommon xs ys in (x : c, r1, r2)
    stripCommon xs ys = ([], xs, ys)
