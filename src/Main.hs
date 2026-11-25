module Main where

import ArgumentsMain (Output (..), OutputFormat (..), Input (..), Arguments (Arguments), arguments, StrategyType (..), InputType (..))
import Compiler.CoreIR (prettyCoreProgram)
import Compiler.CoreIRToForSyDeIR (translateCoreProgram)
import Compiler.ForSyDeIR (prettyIRJSON, prettyIRSystem)
import Compiler.ForSyDeIRToProceduralIR (translateIRSystemToProgram)
import Compiler.GHCCoreAPI (compileToCoreWithForSyDePath)
import Compiler.ProceduralIR (prettyProgram)
import Compiler.ProceduralIRToC (translateProgram)
import Compiler.SchedulerMain (generateSchedule, prettySchedule)
import Compiler.Scheduler.Common (SchedulingStrategy (..), Schedule (..))
import Options.Applicative
import Utilities.OutputHandler (write_output, setupCOutput)

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
