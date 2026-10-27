{-# LANGUAGE LambdaCase #-}

module API
  ( Config(..),
    defaultConfig,
    CompileOptions(..),
    CompileError(..),
    CompileResult(..),
    compileFile,
    getSelectedSpans,
  )
where

import qualified Control.Exception as E
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import ForSyDeIR
import Utilities (compileToCoreWithForSyDePath)
import SDFSchedule (Schedule (..), computeScheduleAndBuffers)
import CoreIRToForSyDeIR (translateCoreProgram)


data Config = Config
  { file :: Maybe FilePath,
    clientId :: Maybe T.Text,
    system :: Maybe IRSystem,
    schedule :: Maybe Schedule,
    forSyDePkg :: Maybe FilePath
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { file = Nothing,
      clientId = Nothing,
      system = Nothing,
      schedule = Nothing,
      forSyDePkg = Nothing
    }

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
            Right resSchedule ->
              pure $ Right CompileResult
                { resultIR = ir,
                  resultSchedule = resSchedule }

getSelectedSpans :: [String] -> Config -> [IRSpan]
getSelectedSpans sel Config {system = ir} = case ir of
  Nothing -> []
  Just (IRSystem _ procs sigs _) ->
    let s = mconcat $ findSignalSpan . IRString <$> sel <*> [sigs]
        a = mconcat $ findProcessSpan . IRString <$> sel <*> [procs]
     in s <> a
  where
    findSignalSpan :: IRId -> [IRSignal] -> [IRSpan]
    findSignalSpan sig l = mapMaybe match l
      where
        match (IRSignal n _ _) = if sig == n then varToSpan n else Nothing
    findProcessSpan :: IRId -> [IRConstructor] -> [IRSpan]
    findProcessSpan proc l = mapMaybe match l
      where
        match = \case
          IRDelay n _ _ -> if proc == n then varToSpan n else Nothing
          IRActor n _ _ _ -> if proc == n then varToSpan n else Nothing
