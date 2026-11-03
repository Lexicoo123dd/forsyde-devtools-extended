module Main where

import ArgumentsMain
import CoreIR (prettyCoreProgram)
import CoreIRToForSyDeIR (translateCoreProgram)
import ForSyDeIR (prettyIRJSON, prettyIRSystem)
import ForSyDeIRToProceduralIR (translateIRSystemToProgram)
import Options.Applicative
import ProceduralIR (prettyProgram)
import ProceduralIRToC (translateProgram)
import SDFSchedule
  ( Schedule (..),
    computeScheduleAndBuffers,
    computeScheduleAndBuffersMCPOC,
    computeScheduleAndBuffersMCPOCWithCores,
    computeScheduleAndBuffersMCPOCPrint,
    computeScheduleAndBuffersMCPOCPrintWithCores,
    computeScheduleAndBuffersPrint,
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
      isMC = multicore args || out_fmt == OutputCMC || out_fmt == OutputScheduleMC
      cores = numCores args
      schedPrint = printSchedule args

  (core, dflags) <- compileToCoreWithForSyDePath pkgPath input_file
  let (forsydeIR, lookupSignals) = case translateCoreProgram dflags core of
        Left e -> error e
        Right v -> v

  -- 1. Check if schedule printing was requested via --print-schedule
  case schedPrint of
    SchedulePrintStdOut -> do
      let schedStr =
            if isMC
              then computeScheduleAndBuffersMCPOCPrintWithCores cores forsydeIR
              else computeScheduleAndBuffersPrint forsydeIR
      putStr schedStr
    SchedulePrintFile path -> do
      let schedStr =
            if isMC
              then computeScheduleAndBuffersMCPOCPrintWithCores cores forsydeIR
              else computeScheduleAndBuffersPrint forsydeIR
      writeFile path schedStr
    NoSchedulePrint -> pure ()

  -- 2. Execute output according to output format
  case out_fmt of
    OutputSchedule ->
      if isMC
        then write_output output_file OutputSchedule (computeScheduleAndBuffersMCPOCPrintWithCores cores forsydeIR)
        else write_output output_file OutputSchedule (computeScheduleAndBuffersPrint forsydeIR)

    OutputScheduleMC ->
      write_output output_file OutputScheduleMC (computeScheduleAndBuffersMCPOCPrintWithCores cores forsydeIR)

    OutputC ->
      if isMC
        then do
          let Schedule (schedule, buffers, delayBuffers) = case computeScheduleAndBuffersMCPOCWithCores cores forsydeIR of
                Left e -> error e
                Right v -> v
          let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals inputType r forsydeIR
          let c = translateProgram proceduralIR t inputType True
          write_output output_file OutputC c
        else do
          let Schedule (schedule, buffers, delayBuffers) = case computeScheduleAndBuffers forsydeIR of
                Left e -> error e
                Right v -> v
          let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals inputType r forsydeIR
          let c = translateProgram proceduralIR t inputType True
          write_output output_file OutputC c

    OutputCMC -> do
      let Schedule (schedule, buffers, delayBuffers) = case computeScheduleAndBuffersMCPOCWithCores cores forsydeIR of
            Left e -> error e
            Right v -> v
      let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals inputType r forsydeIR
      let c = translateProgram proceduralIR t inputType True
      write_output output_file OutputCMC c

    OutputCore ->
      write_output output_file OutputCore (prettyCoreProgram dflags core)

    OutputForSyDeIR ->
      write_output output_file OutputForSyDeIR (prettyIRSystem dflags forsydeIR)

    OutputForSyDeIRJSON ->
      write_output output_file OutputForSyDeIRJSON (prettyIRJSON forsydeIR)

    OutputProceduralIR -> do
      let Schedule (schedule, buffers, delayBuffers) =
            if isMC
              then case computeScheduleAndBuffersMCPOCWithCores cores forsydeIR of
                Left e -> error e
                Right v -> v
              else case computeScheduleAndBuffers forsydeIR of
                Left e -> error e
                Right v -> v
      let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals inputType r forsydeIR
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
