module Compiler.ForSyDeIO where

import Compiler.ForSyDeIR
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

prettySignalField :: IRId -> [IRSignal] -> Bool -> String -> String
prettySignalField signal signals useTargetRate indent =
  printf "%s\"%s\": %d_i"
    indent
    (show signal)
    (if useTargetRate then targetRate else sourceRate)
  where
    IRSignal _ (_, sourceRate) (_, targetRate) =
      head (filter (\(IRSignal signalId _ _) -> signalId == signal) signals)

prettyProCon :: [IRId] -> [IRSignal] -> Bool -> String -> String
prettyProCon signalIds signalIRs isProducer indent =
  printf "%s\"%s\": {\n\
          \%s\n\
          \%s}"
    indent
    text
    (intercalate "\n" (map (\s -> prettySignalField s signalIRs useTargetRate indent2) signalIds))
    indent
  where
    text = if isProducer then "production" else "consumption"
    useTargetRate = if isProducer then False else True
    indent2 = "  " ++ indent

prettyConstructor :: IRConstructor -> [IRSignal] -> String -> Maybe String
prettyConstructor (IRActor actorId _type _functionId (inputSignals, outputSignals)) signals indent =
  Just $ printf "%svertex \"%s\"\n\
                 \%s[moc::sdf::SDFActor]\n\
                 \%s(combFunctions, %s, %s)\n\
                 \%s{\n\
                 \%s\n\
                 \%s\n\
                 \%s}"
    indent
    (show actorId)
    indent
    indent
    (intercalate ", " (map show inputSignals))
    (intercalate ", " (map show outputSignals))
    indent
    (prettyProCon inputSignals signals False ("  " ++ indent))
    (prettyProCon outputSignals signals True ("  " ++ indent))
    indent
prettyConstructor (IRDelay _ _ (_, _)) _ _ = Nothing

-- Input and output signals should not be part of the signal vertices in ForSyDeIO
prettySignal :: IRSignal -> [IRConstructor] -> String -> Maybe String
prettySignal (IRSignal signalId _ _) constructors indent
  -- Skips printing input and output signals
  | any (`isPrefixOf` signalIdText) ["s_in", "s_out"] = Nothing
  -- Avoids printing double signals for signals with delays,
  -- ie skips printing signals matching the output of a delay
  | any matchesOutput constructors = Nothing
  | otherwise =
    Just $ printf "%svertex \"%s_sig\"\n\
                   \%s[moc::sdf::SDFChannel]\n\
                   \%s(producer, consumer, initialTokenValues)\n\
                   \%s{\n\
                   \%s\"numOfInitialTokens\": %d_i\n\
                   \%s}"
      indent
      signalIdText
      indent
      indent
      indent
      indent2
      delayCount
      indent
  where
    signalIdText = show signalId
    matchesOutput (IRDelay _ _ (_, outputSignal)) =
      outputSignal == signalId
    matchesOutput _ = False
    -- A signal has a delay if there exists an IRDelay constructor with the signal as an input
    -- That IRDelay has a list of those delays ("tokens")
    delayCount = case find matches constructors of
      Just (IRDelay _ tokens _) -> length tokens
      _ -> 0
    matches (IRDelay _ _ (inputSignal, _)) = inputSignal == signalId
    matches _ = False
    indent2 = "  " ++ indent

prettyIOGlobal :: IRSystem -> String
prettyIOGlobal (IRSystem (_globalinputs, _globaloutputs) constructors signals _functions) =
  let indent = "  " in
  printf "systemgraph {\n\
          \%s\n\n\
          \%s\n\
          \}"
    (intercalate "\n" (mapMaybe (\c -> prettyConstructor c signals indent) constructors)) 
    (intercalate "\n" (mapMaybe (\s -> prettySignal s constructors indent) signals)) 
