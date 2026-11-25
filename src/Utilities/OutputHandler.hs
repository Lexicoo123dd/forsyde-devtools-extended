{-# LANGUAGE ScopedTypeVariables #-}
module Utilities.OutputHandler (write_output, setupCOutput) where

import ArgumentsMain (Output (..), HasExtension (..), OutputFormat (..), InputType (..))
import Compiler.ForSyDeIR (IRSystem)
import Compiler.ForSyDeIRToProceduralIR (generateInputHeader)
import Compiler.Scheduler.Common (Schedule (..))
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
