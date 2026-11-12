{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ArgumentsMain
import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import CoreIR (prettyCoreProgram)
import CoreIRToForSyDeIR (translateCoreProgram)
import ForSyDeIR (prettyIRJSON, prettyIRSystem)
import ForSyDeIRToProceduralIR
  ( generateInputHeader,
    translateScheduleToProgram,
  )
import Options.Applicative
import ProceduralIR (prettyProgram)
import ProceduralIRToC (translateProgram)
import Schedule
  ( Schedule (..),
    generateSchedule,
    prettySchedule,
  )
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
import Utilities (compileToCoreWithForSyDePath)

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
-- Need to pattern match based on the arguments used. They are matched on
-- manually since a lot of combinations are invalid.
-- You can't for intance start with procedural IR as input and go to Core
-- since the compiler doesn't go backwards.

run :: Arguments -> IO ()
run args = do
  let InputFile input_file = input args
      output_file = output args
      out_fmt = output_format args
      t = target args
      inputType = iType args
      r = runs args
      pkgPath = forSyDePath args
      strat = scheduleStrategy args
      schedPrint = printSchedule args

  (core, dflags) <- compileToCoreWithForSyDePath pkgPath input_file
  let (forsydeIR, lookupSignals) = case translateCoreProgram dflags core of
        Left e -> error e
        Right v -> v

  -- Generate schedule using the selected strategy
  let schedule = case generateSchedule strat forsydeIR of
        Left e -> error e
        Right s -> s

  -- 1. Check if schedule printing was requested via -s / --print-schedule
  case schedPrint of
    SchedulePrintStdOut -> putStr (prettySchedule forsydeIR schedule)
    SchedulePrintFile path -> writeFile path (prettySchedule forsydeIR schedule)
    NoSchedulePrint -> pure ()

  -- 2. Execute output according to output format
  case out_fmt of
    OutputSchedule ->
      write_output output_file OutputSchedule (prettySchedule forsydeIR schedule)

    OutputC -> do
      let isMultiCore = length (schedCores schedule) > 1
      let effectiveInputType = if isMultiCore then Predefined else inputType
      let isBench = benchmark args
      case output_file of
        StdOut -> do
          let proceduralIR = translateScheduleToProgram dflags schedule lookupSignals t effectiveInputType r isBench forsydeIR
          let c = translateProgram proceduralIR t effectiveInputType True
          putStr c
        _ -> do
          let (targetDir, cFilePath) = resolveCOutput input_file output_file
          createDirectoryIfMissing True targetDir
          when (isMultiCore || effectiveInputType == Predefined) $ do
            let inputHeaderPath = targetDir </> "input.h"
            headerExists <- doesFileExist inputHeaderPath
            unless headerExists $
              writeFile inputHeaderPath (generateInputHeader forsydeIR schedule)
          setupIncludeSymlink targetDir
          let proceduralIR = translateScheduleToProgram dflags schedule lookupSignals t effectiveInputType r isBench forsydeIR
          let c = translateProgram proceduralIR t effectiveInputType True
          writeFile cFilePath c

    OutputCore ->
      write_output output_file OutputCore (prettyCoreProgram dflags core)

    OutputForSyDeIR ->
      write_output output_file OutputForSyDeIR (prettyIRSystem dflags forsydeIR)

    OutputForSyDeIRJSON ->
      write_output output_file OutputForSyDeIRJSON (prettyIRJSON forsydeIR)

    OutputProceduralIR -> do
      let isMultiCore = length (schedCores schedule) > 1
      let effectiveInputType = if isMultiCore then Predefined else inputType
      let isBench = benchmark args
      let proceduralIR = translateScheduleToProgram dflags schedule lookupSignals t effectiveInputType r isBench forsydeIR
      write_output output_file OutputProceduralIR (prettyProgram proceduralIR)

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
              then let outDir = dropExtension (takeFileName rawPath) in (outDir, outDir </> takeFileName rawPath)
              else (dir, rawPath)
          else
            let targetDir = case (rawPath, hasExt) of
                  ("main", NoFileExtension) -> stem
                  _ -> rawPath
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
          let legacyCand = dir </> "examples" </> "implementation" </> "platform_independent" </> "include"
          legacyEx <- doesFileExist (legacyCand </> "common.h")
          if legacyEx
            then pure (Just legacyCand)
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
