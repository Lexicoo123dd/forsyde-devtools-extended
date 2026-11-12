module ArgumentsMain where

import Data.Char (toLower)
import Options.Applicative
import Schedule (SchedulingStrategy (..))

-- Which file to use for input
data Input
  = InputFile FilePath

-- Output, can either be a file or stdout
data Output
  = OutputFile (FilePath, HasExtension)
  | StdOut

data HasExtension
  = WithFileExtension
  | NoFileExtension
  deriving (Show, Eq)

-- Stubbed out this type for now, will probably not implement
-- reading separate input formats
data InputFormat
  = InputForSyDe
  | InputCore
  | InputForSyDeIR
  | InputForSyDeIRJSON
  | InputProceduralIR

data OutputFormat
  = OutputC
  | OutputCMC
  | OutputCore
  | OutputForSyDeIR
  | OutputForSyDeIRJSON
  | OutputProceduralIR
  | OutputSchedule
  | OutputScheduleMC
  deriving (Show, Eq)

data Target
  = PC
  | PICO2
  deriving (Show, Eq)

data InputType
  = StdIn
  | Predefined
  deriving (Show, Eq)

data Runs
  = Limited Int
  | Perpetual
  deriving (Show, Eq)

data SchedulePrint
  = NoSchedulePrint
  | SchedulePrintStdOut
  | SchedulePrintFile FilePath
  deriving (Show, Eq)

data Arguments = Arguments
  { -- Files
    input :: Input,
    output :: Output,
    -- Formats
    output_format :: OutputFormat,
    -- Target
    target :: Target,
    -- Input Format (whether to get SDF Input from scanf or predetermined file)
    iType :: InputType,
    runs :: Runs,
    -- Path to the package DB containing ForSyDe Shallow
    forSyDePath :: Maybe FilePath,
    -- Scheduling strategy
    scheduleStrategy :: SchedulingStrategy,
    printSchedule :: SchedulePrint,
    numCores :: Int,
    benchmark :: Bool
  }

-- Handle file input, always need to define a file
inputFile :: Parser Input
inputFile =
  InputFile
    <$> strArgument
      ( metavar "INPUT"
          <> help "Input filename"
      )

outputFileTop :: Parser Output
outputFileTop =
  outputFile
    <|> outputFileStdOut

-- Function to create a filepath with file extension
fileName :: ReadM (FilePath, HasExtension)
fileName = str >>= returnFile
  where
    returnFile s = pure (s, WithFileExtension)

-- Output to file optionally, otherwise stdout by default
outputFile :: Parser Output
outputFile =
  option
    (OutputFile <$> fileName)
    ( short 'o'
        <> long "output"
        <> metavar "OUTPUT"
        <> value (OutputFile ("main", NoFileExtension))
        <> help "Output filename"
    )

outputFileStdOut :: Parser Output
outputFileStdOut =
  flag'
    StdOut
    ( long "stdout"
        <> help "Print output to stdout"
    )

-- Top-level. All of these 4 flags are optional, but one of them
-- need to be active. How does that work? inputFormatCore has a
-- default value for InputForSyde if no flag is provided
inputFormatTop :: Parser InputFormat
inputFormatTop =
  inputFormatCore
    <|> inputFormatForSyDeIR
    <|> inputFormatProceduralIR

inputFormatCore :: Parser InputFormat
inputFormatCore =
  flag
    InputForSyDe
    InputCore
    ( long "input-core"
        <> help "Input file in ForSyDe"
    )

inputFormatForSyDeIR :: Parser InputFormat
inputFormatForSyDeIR =
  flag'
    InputForSyDeIR
    ( long "input-forsyde-ir"
        <> help "Input file in ForSyDe-IR"
    )

inputFormatProceduralIR :: Parser InputFormat
inputFormatProceduralIR =
  flag'
    InputProceduralIR
    ( long "input-procedural-ir"
        <> help "Input file in Procedural-IR"
    )

-- Top-level. All of these flags are optional, but one of them
-- need to be active.
outputFormatTop :: Parser OutputFormat
outputFormatTop =
  outputFormatScheduleMC
    <|> outputFormatCMCPOC
    <|> outputFormatCore
    <|> outputFormatForSyDeIR
    <|> outputFormatForSyDeIRJSON
    <|> outputFormatProceduralIR
    <|> outputFormatSchedule
    <|> outputFormatC

outputFormatScheduleMC :: Parser OutputFormat
outputFormatScheduleMC =
  flag'
    OutputScheduleMC
    ( long "output-schedule-mc"
        <> long "schedule-mc"
        <> long "output-mc-schedule"
        <> help "Output the multi-core SDF schedule per core to file instead of code"
    )

outputFormatC :: Parser OutputFormat
outputFormatC =
  flag
    OutputC
    OutputC
    ( long "output-c"
        <> help "Output file in C (default)"
    )

outputFormatCMCPOC :: Parser OutputFormat
outputFormatCMCPOC =
  flag'
    OutputCMC
    ( long "output-c-mc"
        <> help "Output file in C multi-core"
    )

outputFormatCore :: Parser OutputFormat
outputFormatCore =
  flag'
    OutputCore
    ( long "output-core"
        <> help "Output file in Core"
    )

outputFormatForSyDeIR :: Parser OutputFormat
outputFormatForSyDeIR =
  flag'
    OutputForSyDeIR
    ( long "output-forsyde-ir"
        <> help "Output file in ForSyDe-IR"
    )

outputFormatForSyDeIRJSON :: Parser OutputFormat
outputFormatForSyDeIRJSON =
  flag'
    OutputForSyDeIRJSON
    ( long "output-forsyde-ir-json"
        <> help "Output file in ForSyDe-IR-JSON"
    )

outputFormatProceduralIR :: Parser OutputFormat
outputFormatProceduralIR =
  flag'
    OutputProceduralIR
    ( long "output-procedural-ir"
        <> help "Output file in Procedural-IR"
    )

outputFormatSchedule :: Parser OutputFormat
outputFormatSchedule =
  flag'
    OutputSchedule
    ( long "output-schedule"
        <> help "Output the SDF schedule to file instead of code"
    )

targetName :: ReadM Target
targetName = str >>= returnTarget
  where
    returnTarget s = case s of
      "PC" -> pure (PC)
      "PICO2" -> pure (PICO2)
      t -> error ("Unsupported target: " ++ t)

-- Parse target platform. Uses pattern matching to handle raw strings which
-- means it is used  like "--target={target}" as opposed to having --target-PC,
-- target-PICO2, etc
targetTop :: Parser Target
targetTop =
  option
    (targetName)
    ( short 't'
        <> long "target"
        <> metavar "TARGET"
        <> value PC
        <> help "Target platform for C code. (PC default, PC and PICO2 supported)"
    )

-- Parse InputType. Implementation is similar to `target` option
ioName :: ReadM InputType
ioName = str >>= returnIO
  where
    returnIO s = case s of
      "stdin" -> pure (StdIn)
      "predefined" -> pure (Predefined)
      t -> error ("Unsupported input source: " ++ t)

-- Parse target platform. Uses pattern matching to handle raw strings which
-- means it is used  like "--target={target}" as opposed to having --target-PC,
-- target-PICO2, etc
ioTop :: Parser InputType
ioTop =
  option
    (ioName)
    ( long "input-type"
        <> metavar "INPUTTYPE"
        <> value StdIn
        <> help "Source of input tokens for the SDF models in the C code. (stdin default, stdin and predefined supported)"
    )

runsParse :: ReadM Runs
runsParse = str >>= returnRuns
  where
    returnRuns s = case s of
      "inf" -> pure (Perpetual)
      x -> pure (Limited (read x))

runsTop :: Parser Runs
runsTop =
  option
    (runsParse)
    ( long "runs"
        <> metavar "RUNS"
        <> value (Limited 1)
        <> help "How many times to loop input data, when input mode is set to 'predefined'. 1 By default, pass an integer for a limited number or 'inf' to run the program perpetually"
    )

-- Path to the package DB containing ForSyDe Shallow
forSyDePathOption :: Parser (Maybe FilePath)
forSyDePathOption =
  option
    (str >>= \s -> pure $ Just s)
    ( long "forsyde-pkgpath"
        <> value Nothing
        <> help "The path to the ForSyDe Shallow package (unspecified by default)"
    )

data StrategyType
  = StrategyGreedy
  | StrategyHuLevel
  deriving (Show, Eq)

-- Flag for scheduling strategy: defaults to greedy.
scheduleStrategyOption :: Parser StrategyType
scheduleStrategyOption =
  option
    (str >>= parseStrategy)
    ( long "scheduleStrategy"
        <> long "schedule-strategy"
        <> long "strategy"
        <> metavar "STRATEGY"
        <> value StrategyGreedy
        <> help "Scheduling strategy to use: greedy (default), hu-level"
    )
  where
    parseStrategy s = case map toLower s of
      "greedy" -> pure StrategyGreedy
      "hu-level" -> pure StrategyHuLevel
      "hulevel" -> pure StrategyHuLevel
      "hu" -> pure StrategyHuLevel
      other -> readerError ("Unknown scheduling strategy '" ++ other ++ "'. Supported strategies: greedy, hu-level")

-- Optional flag for schedule printing: defaults to stdout if -s or --print-schedule is given. Default when omitted is not to print (NoSchedulePrint).
schedulePrintOption :: Parser SchedulePrint
schedulePrintOption =
  flag' SchedulePrintStdOut
    ( short 's'
        <> long "print-schedule"
        <> long "print-schedule-stdout"
        <> long "schedule-stdout"
        <> help "Print the SDF schedule to stdout (default when -s or --print-schedule is specified)"
    )
    <|> option
      ( str >>= \s -> case s of
          "stdout" -> pure SchedulePrintStdOut
          "-" -> pure SchedulePrintStdOut
          f -> pure (SchedulePrintFile f)
      )
      ( long "print-schedule"
          <> long "print-schedule-file"
          <> long "schedule-file"
          <> metavar "FILE"
          <> help "Print schedule to a specified file (e.g. --print-schedule=FILE or --schedule-file FILE)"
      )
    <|> pure NoSchedulePrint

-- Optional flag for number of cores: default is 2 cores.
numCoresOption :: Parser Int
numCoresOption =
  option
    auto
    ( short 'c'
        <> long "cores"
        <> long "num-cores"
        <> metavar "CORES"
        <> value 2
        <> showDefault
        <> help "Number of cores to use for multi-core scheduling (default: 2)"
    )

-- Optional flag to include BENCH_START() and BENCH_END() timing hooks in generated C.
benchmarkOption :: Parser Bool
benchmarkOption =
  switch
    ( long "benchmark"
        <> long "timing"
        <> long "bench"
        <> help "Include BENCH_START() and BENCH_END() timing hooks in generated C code (disabled by default)"
    )

-- Top level argument parsing function
arguments :: Parser Arguments
arguments =
  assembleArguments
    <$> inputFile
    <*> outputFileTop
    <*> outputFormatTop
    <*> targetTop
    <*> ioTop
    <*> runsTop
    <*> forSyDePathOption
    <*> scheduleStrategyOption
    <*> schedulePrintOption
    <*> numCoresOption
    <*> benchmarkOption
  where
    assembleArguments inF outF outFmt tgt io r pkg stratType printSched nCores bench =
      Arguments
        { input = inF,
          output = outF,
          output_format = outFmt,
          target = tgt,
          iType = io,
          runs = r,
          forSyDePath = pkg,
          scheduleStrategy = case stratType of
            StrategyGreedy -> Greedy
            StrategyHuLevel -> HuLevel nCores,
          printSchedule = printSched,
          numCores = nCores,
          benchmark = bench
        }
