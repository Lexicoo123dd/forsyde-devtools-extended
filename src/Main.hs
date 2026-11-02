module Main where

import ArgumentsMain (Output (..), HasExtension (..), OutputFormat (..), Input (..), Arguments (Arguments), arguments)
import CoreIR (prettyCoreProgram)
import CoreIRToForSyDeIR (translateCoreProgram)
import ForSyDeIR (prettyIRJSON, prettyIRSystem)
import ForSyDeIRToProceduralIR (translateIRSystemToProgram)
import Options.Applicative
import ProceduralIR (prettyProgram)
import ProceduralIRToC (translateProgram)
import SDFSchedule (Schedule (..), computeScheduleAndBuffers, computeScheduleAndBuffersPrint)
import GHCCoreAPI (compileToCoreWithForSyDePath)

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
run (Arguments (InputFile input_file) output_file outputType target inputType runs pkgPath) = do
  (core, dflags) <- compileToCoreWithForSyDePath pkgPath input_file
  let outputIR = case outputType of
        OutputCore ->
          prettyCoreProgram dflags core
        OutputForSyDeIR -> do
          let (forsydeIR, _) = translateCoreHelper dflags core
          prettyIRSystem dflags forsydeIR
        OutputForSyDeIRJSON -> do
          let (forsydeIR, _) = translateCoreHelper dflags core
          prettyIRJSON forsydeIR
        OutputSchedule -> do
          let (forsydeIR, _) = translateCoreHelper dflags core
          computeScheduleAndBuffersPrint forsydeIR
        OutputProceduralIR -> do
          let (forsydeIR, lookupSignals) = translateCoreHelper dflags core
          let (schedule, buffers, delayBuffers) = computeScheduleHelper forsydeIR
          prettyProgram $ translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals inputType runs forsydeIR
        OutputC -> do
          let (forsydeIR, lookupSignals) = translateCoreHelper dflags core
          let (schedule, buffers, delayBuffers) = computeScheduleHelper forsydeIR
          let proceduralIR = translateIRSystemToProgram dflags schedule buffers delayBuffers lookupSignals inputType runs forsydeIR
          translateProgram proceduralIR target inputType True
  write_output output_file outputType outputIR
  where
    translateCoreHelper dflags core = case translateCoreProgram dflags core of
      Left e -> error e
      Right v -> v
    computeScheduleHelper forsydeIR = case computeScheduleAndBuffers forsydeIR of
      Left e -> error e
      Right (Schedule v) -> v

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
