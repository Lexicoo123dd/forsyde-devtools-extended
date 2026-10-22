module API
  ( CompileOptions(..),
    CompileError(..),
    CompileResult(..),
    compileFile,
  )
where

import qualified Control.Exception as E
import ForSyDeIR
import Utilities (compileToCoreWithForSyDePath)
import SDFSchedule (Schedule (..), computeScheduleAndBuffers)
import CoreIRToForSyDeIR (translateCoreProgram)

data CompileOptions = CompileOptions
  { forSyDePath :: Maybe FilePath
  }

data CompileError
  = CoreCompilationFailed String
  | IRTranslationFailed String
  | SchedulingFailed String
  deriving (Show, Eq)

data CompileResult = CompileResult
  { resultIR :: IRSystem,
    resultSchedule:: Schedule
  }

compileFile
  :: CompileOptions
  -> FilePath
  -> IO (Either CompileError CompileResult)
compileFile options sourceFile = do
  coreResult <- E.try $ compileToCoreWithForSyDePath (forSyDePath options) sourceFile
  case coreResult of
    Left e ->
      pure $ Left $ CoreCompilationFailed (show (e :: E.SomeException))
    Right (core, dflags) ->
      case translateCoreProgram dflags core of
        Left e ->
          pure $ Left $ IRTranslationFailed e
        Right (ir, _) ->
          case computeScheduleAndBuffers ir of
            Left e ->
              pure $ Left $ SchedulingFailed e
            Right schedule ->
              pure $ Right CompileResult
                { resultIR = ir,
                  resultSchedule = schedule }
