module Compiler.ForSyDeIO (prettyForSyDeIO) where

import Compiler.CoreIRToProceduralIR (FunctionContent (..), translateIRFunctionPartial)
import Compiler.ForSyDeIR
import Data.Bits (finiteBitSize)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import GHC (DynFlags)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

-- Converts a list of FunctionContent e.g. [FPlus, FPlus, FDiv, FMinus, FDiv, FVar someVar, FInt 5]
-- to a map counting the operators, in this example to [("integer add", 2), ("integer div", 2), ("integer subtract", 1)]
functionListToMap :: [FunctionContent] -> Map.Map String Int
functionListToMap = foldr addToMap Map.empty
  where
    addToMap content acc = case content of
      FPlus -> increment "integer add" acc
      FMinus -> increment "integer sub" acc
      FMultiply -> increment "integer mul" acc
      FDiv -> increment "integer div" acc
      _ -> acc
    increment key = Map.insertWith (+) key 1

prettyFunction :: [FunctionContent] -> Doc ann
prettyFunction functionList =
  vsep
    [ dquotes (pretty "operationRequirements") <> colon <+> lbrace,
      indent 2 $ vsep
        [ dquotes (pretty "default") <> colon <+> lbrace,
          indent 2 $ vsep $ punctuate comma functionListPretty,
          rbrace
        ],
      rbrace
    ]
  where
    -- Converts a map in the form of e.g. [("integer add", 1), ("integer sub", 2)]
    -- to a prettied list ["integer add: 1_i", "integer sub: 2_i"]
    functionListPretty = map (\(key, value) -> dquotes (pretty key) <> colon <+> pretty value <> pretty "_i") (Map.toList functionMap)
    functionMap = functionListToMap functionList

prettySignalField :: IRId -> [IRSignal] -> Bool -> Doc ann
prettySignalField signal signals useTargetRate =
  dquotes (pretty $ show signal) <> colon <+> pretty rate <> pretty "_i"
  where
    rate = if useTargetRate then targetRate else sourceRate
    -- Finds the signal among all signal IRs to extract its source and target rate
    -- Don't think a missing signal should be possible, so it throws an error
    IRSignal _ (_, sourceRate) (_, targetRate) =
      case find (\(IRSignal signalId _ _) -> signalId == signal) signals of
        Just s -> s
        Nothing -> error ("Signal not found: " ++ show signal)

prettyProCon :: [IRId] -> [IRSignal] -> Bool -> Doc ann
prettyProCon signalIds signalIRs isProducer =
  vsep
    [ dquotes (pretty text) <> colon <+> lbrace,
      indent 2 (vsep fields),
      rbrace
    ]
  where
    text = if isProducer then "production" else "consumption"
    useTargetRate = not isProducer
    fields = punctuate comma (map (\s -> prettySignalField s signalIRs useTargetRate) signalIds)

prettyConstructor :: IRConstructor -> [IRSignal] -> [IRFunction] -> DynFlags -> Maybe (Doc ann)
prettyConstructor (IRDelay _ _ (_, _)) _ _ _ = Nothing
prettyConstructor actor@(IRActor actorId _type functionId (inputSignals, outputSignals)) signals functions dflags =
  Just $ vsep
    [ pretty "vertex" <+> dquotes (pretty (show actorId)),
      pretty "[" <> functionTrait <> pretty "moc::sdf::SDFActor, moc::sdf::SDFComb, visualization::Visualizable]",
      parens $ hsep $ punctuate comma
        [ pretty "combFunctions",
          hsep $ punctuate comma $ map (pretty . show) inputSignals,
          hsep $ punctuate comma $ map (pretty . show) outputSignals
        ],
      lbrace,
      indent 2 $ vsep $ punctuate comma $
        [ prettyProCon inputSignals signals False,
          prettyProCon outputSignals signals True
        ] ++ prettiedFunction,
      rbrace
    ]
  where
    function = case find (\(IRFunction fid _) -> fid == functionId) functions of
      Just (IRFunction _ expr) -> expr
      Nothing -> Nothing
    (functionTrait, prettiedFunction) = case function of
      Nothing -> (mempty, [])
      Just expr ->
        let functionList = translateIRFunctionPartial dflags actor expr
        in (pretty "impl::InstrumentedExecutable, ", [prettyFunction functionList])

prettySignal :: IRId -> [IRConstructor] -> Doc ann
prettySignal signalId constructors =
    vsep
      [ pretty "vertex" <+> dquotes (pretty $ show signalId <> "_sig"),
        pretty "[moc::sdf::SDFChannel, visualization::Visualizable]",
        pretty "(producer, consumer, initialTokenValues)",
        lbrace,
        indent 2 $ vsep $ punctuate comma
          [ dquotes (pretty "numOfInitialTokens") <> colon <+> delayCount <> pretty "_i",
            dquotes (pretty "tokenSizeInBits") <> colon <+> pretty (finiteBitSize (0 :: Int)) <> pretty "_l"
          ],
        rbrace
      ]
  where
    -- A signal has a delay if there exists an IRDelay constructor with the signal as an input
    -- That IRDelay has a list of those delays ("tokens")
    delayCount = pretty $ case find matches constructors of
      Just (IRDelay _ tokens _) -> length tokens
      _ -> 0
    matches (IRDelay _ _ (inputSignal, _)) = inputSignal == signalId
    matches _ = False

prettyActorEdge :: IRId -> IRId -> [IRId] -> Maybe (Doc ann)
prettyActorEdge actorId outputSignalId globalOutputs
  -- Skip creating edges to global outputs
  | any (==outputSignalId) globalOutputs = Nothing
  | otherwise =
    Just $ pretty "edge [moc::sdf::SDFDataEdge, visualization::VisualConnection]" <+> fromActor <+> fromPort <+> toActor <+> toPort
    where
      fromActor = pretty "from" <+> dquotes (pretty $ show actorId)
      fromPort = pretty "port" <+> dquotes (pretty $ show outputSignalId)
      toActor = pretty "to" <+> dquotes (pretty $ show outputSignalId <> "_sig")
      toPort = pretty "port" <+> dquotes (pretty "producer")

prettySignalEdge :: IRId -> IRId -> IRId -> Doc ann
prettySignalEdge signalId outputActorId outputPort =
  pretty "edge [moc::sdf::SDFDataEdge, visualization::VisualConnection]" <+> fromActor <+> fromPort <+> toActor <+> toPort
  where
    fromActor = pretty "from" <+> dquotes (pretty $ show signalId <> "_sig")
    fromPort = pretty "port" <+> dquotes (pretty "consumer")
    toActor = pretty "to" <+> dquotes (pretty $ show outputActorId)
    toPort = pretty "port" <+> dquotes (pretty $ show outputPort)

prettyForSyDeIO :: DynFlags -> IRSystem -> String
prettyForSyDeIO dflags (IRSystem (globalInputs, globalOutputs) constructors signals functions) =
  let document = vsep
        [ pretty "systemgraph" <+> lbrace,
          indent 2 $ vsep
            [ vsep $ mapMaybe (\c -> prettyConstructor c signals functions dflags) constructors,
              vsep $ map (\(s, _) -> prettySignal s constructors) signalEdges,
              vsep $ mapMaybe (\(c, s) -> prettyActorEdge c s globalOutputs) actorEdges,
              vsep $ map (\(s, (t, p)) -> prettySignalEdge s t p) signalEdges
            ],
          rbrace,
          mempty
        ]
  in renderString $ layoutPretty defaultLayoutOptions document
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
