module Main where

import ArgumentsMain
import Control.Monad (unless, when)
import CoreIR (prettyCoreProgram)
import CoreIRToForSyDeIR (translateCoreProgram)
import ForSyDeIR (prettyIRJSON, prettyIRSystem)
import ForSyDeIRToProceduralIR
  ( generateInputHeader,
    translateIRSystemToProgram,
    translateScheduleToProgram,
  )
import Options.Applicative
import ProceduralIR (prettyProgram)
import ProceduralIRToC (translateProgram)
import Schedule
  ( Schedule (..),
    generateSchedule,
    prettySchedule,
    toFlatSchedule,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
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
generateDefaultFileName f OutputCMC = f ++ ".c"
generateDefaultFileName f OutputCore = f ++ ".hcr"
generateDefaultFileName f OutputForSyDeIR = f ++ ".fir"
generateDefaultFileName f OutputForSyDeIRJSON = f ++ ".json"
generateDefaultFileName f OutputProceduralIR = f ++ ".pir"
generateDefaultFileName f OutputSchedule = f ++ ".sched"
generateDefaultFileName f OutputScheduleMC = f ++ ".sched"

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

    OutputScheduleMC ->
      write_output output_file OutputScheduleMC (prettySchedule forsydeIR schedule)

    OutputC -> do
      let isMultiCore = length (schedCores schedule) > 1
      let effectiveInputType = if isMultiCore then Predefined else inputType
      when (isMultiCore || effectiveInputType == Predefined) $ do
        let targetDir = case output_file of
              OutputFile (path, _) -> takeDirectory path
              StdOut -> "."
        let inputHeaderPath = targetDir </> "input.h"
        headerExists <- doesFileExist inputHeaderPath
        unless headerExists $
          writeFile inputHeaderPath (generateInputHeader forsydeIR schedule)
      let proceduralIR = translateScheduleToProgram dflags schedule lookupSignals t effectiveInputType r forsydeIR
      let c = translateProgram proceduralIR t effectiveInputType True
      write_output output_file OutputC c

    OutputCMC -> do
      let targetDir = case output_file of
            OutputFile (path, _) -> takeDirectory path
            StdOut -> "."
      let inputHeaderPath = targetDir </> "input.h"
      headerExists <- doesFileExist inputHeaderPath
      unless headerExists $
        writeFile inputHeaderPath (generateInputHeader forsydeIR schedule)
      let proceduralIR = translateScheduleToProgram dflags schedule lookupSignals t Predefined r forsydeIR
      let c = translateProgram proceduralIR t Predefined True
      write_output output_file OutputCMC c

    OutputCore ->
      write_output output_file OutputCore (prettyCoreProgram dflags core)

    OutputForSyDeIR ->
      write_output output_file OutputForSyDeIR (prettyIRSystem dflags forsydeIR)

    OutputForSyDeIRJSON ->
      write_output output_file OutputForSyDeIRJSON (prettyIRJSON forsydeIR)

    OutputProceduralIR -> do
      let flatSchedule = toFlatSchedule schedule
      let proceduralIR = translateIRSystemToProgram dflags flatSchedule (schedBufferSizes schedule) (schedDelayBuffers schedule) lookupSignals inputType r forsydeIR
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
