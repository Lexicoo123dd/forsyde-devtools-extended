module Compiler.ForSyDeIO where

import Compiler.ForSyDeIR
import Data.List (find, intercalate)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

prettySignalField :: IRId -> [IRSignal] -> Bool -> String -> String
prettySignalField signal signals useTargetRate indent =
  printf "%s\"%s\": %d_i"
    indent
    (show signal)
    (if useTargetRate then targetRate else sourceRate)
  where
    -- Finds the signal among all signal IRs to extract its source and target rate
    -- Don't think a missing signal should be possible, so it throws an error
    IRSignal _ (_, sourceRate) (_, targetRate) =
      case find (\(IRSignal signalId _ _) -> signalId == signal) signals of
        Just s -> s
        Nothing -> error ("Signal not found: " ++ show signal)

prettyProCon :: [IRId] -> [IRSignal] -> Bool -> String -> String
prettyProCon signalIds signalIRs isProducer indent =
  printf "%s\"%s\": {\n\
          \%s\n\
          \%s}%s"
    indent
    text
    (intercalate ",\n" (map (\s -> prettySignalField s signalIRs useTargetRate indent2) signalIds))
    indent
    endText
  where
    text = if isProducer then "production" else "consumption"
    useTargetRate = if isProducer then False else True
    endText = if isProducer then "" else ","
    indent2 = "  " ++ indent

prettyConstructor :: IRConstructor -> [IRSignal] -> String -> Maybe String
prettyConstructor (IRDelay _ _ (_, _)) _ _ = Nothing
prettyConstructor (IRActor actorId _type _functionId (inputSignals, outputSignals)) signals indent =
  Just $ printf "%svertex \"%s\"\n\
                 \%s[moc::sdf::SDFActor, visualization::Visualizable]\n\
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

prettySignal :: IRId -> [IRConstructor] -> String -> String
prettySignal signalId constructors indent =
    printf "%svertex \"%s_sig\"\n\
                   \%s[moc::sdf::SDFChannel, visualization::Visualizable]\n\
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
    -- A signal has a delay if there exists an IRDelay constructor with the signal as an input
    -- That IRDelay has a list of those delays ("tokens")
    delayCount = case find matches constructors of
      Just (IRDelay _ tokens _) -> length tokens
      _ -> 0
    matches (IRDelay _ _ (inputSignal, _)) = inputSignal == signalId
    matches _ = False
    indent2 = "  " ++ indent

prettyActorEdge :: IRId -> IRId -> [IRId] -> String -> Maybe String
prettyActorEdge actorId outputSignalId globalOutputs indent
  -- Skip creating edges to global outputs
  | any (==outputSignalId) globalOutputs = Nothing
  | otherwise =
    Just $ printf "%sedge [moc::sdf::SDFDataEdge, visualization::VisualConnection] from \"%s\" port \"%s\" to \"%s\" port \"producer\""
      indent
      (show actorId)
      (show outputSignalId)
      (show outputSignalId ++ "_sig")

prettySignalEdge :: IRId -> IRId -> IRId -> String -> String
prettySignalEdge signalId outputActorId outputPort indent =
    printf "%sedge [moc::sdf::SDFDataEdge, visualization::VisualConnection] from \"%s\" port \"consumer\" to \"%s\" port \"%s\""
      indent
      (show signalId ++ "_sig")
      (show outputActorId)
      (show outputPort)

prettyIOGlobal :: IRSystem -> String
prettyIOGlobal (IRSystem (globalInputs, globalOutputs) constructors signals _functions) =
  let indent = "  " in
  printf "systemgraph {\n\
          \%s\n\n\
          \%s\n\n\
          \%s\n\
          \%s\n\
          \}"
    (intercalate "\n" $ mapMaybe (\c -> prettyConstructor c signals indent) constructors)
    (intercalate "\n" $ map (\(s, _) -> prettySignal s constructors indent) signalEdges)
    (intercalate "\n" $ mapMaybe (\(c, s) -> prettyActorEdge c s globalOutputs indent) actorEdges)
    (intercalate "\n" $ map (\(s, (t, p)) -> prettySignalEdge s t p indent) signalEdges)
  where
    signalEdges = mapMaybe getSignalEdges signals
    actorEdges = concatMap getActorEdges constructors
    -- Should exist an edge for every output signal in every actor
    getActorEdges :: IRConstructor -> [(IRId, IRId)]
    -- Returns an entry for every outputSignal in the actors
    getActorEdges (IRActor actorId _ _ (_, outputSignals)) =
      map (\s -> (actorId, s)) outputSignals
    -- Filters out delays
    getActorEdges (IRDelay _ _ (_, _)) = []
    -- Input and output signals should not be part of the signal vertices in ForSyDeIO
    getSignalEdges :: IRSignal -> Maybe (IRId, (IRId, IRId))
    getSignalEdges (IRSignal signalId _ (targetId, _))
      -- Skips input and output signals
      | any (==signalId) globalInputs = Nothing
      | any (==signalId) globalOutputs = Nothing
      -- Avoids printing double signals for signals with delays,
      -- ie skips printing signals matching the output of a delay
      | any (\c -> case c of
          IRDelay _ _ (_, outputSignal) -> outputSignal == signalId
          IRActor _ _ _ _ -> False
        ) constructors = Nothing
      -- If there is a delay, the target of the signal will be the delay actor instead of the next real actor.
      -- Solution: Find if the target is a delay and if it is, extract its output signal.
      -- Find the signal that matches that output signal, and extract its next target id instead.
      -- If it is a delay, we also need to use the delay's output signal's id as the target port name
      | otherwise = Just (signalId, resolveTarget targetId)
      where
        resolveTarget target =
          case find isTargetDelay constructors of
            Just (IRDelay _ _ (_, outputSignal)) ->
              case find (\(IRSignal sid _ _) -> sid == outputSignal) signals of
                Just (IRSignal _ _ (nextTarget, _)) -> (nextTarget, outputSignal)
                Nothing -> (target, signalId)
            _ -> (target, signalId)
          where
            isTargetDelay (IRDelay delayId _ _) = delayId == target
            isTargetDelay _ = False
