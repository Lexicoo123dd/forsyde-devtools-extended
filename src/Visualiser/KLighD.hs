{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Diagram synthesis for the KLighD renderer.
--
-- This module owns the mapping from ForSyDe IR and analysis results to
-- KLighD's graph schema. Keeping the mapping here lets the language server
-- coordinate protocol messages without also knowing how diagram elements are
-- rendered or laid out.
module Visualiser.KLighD (forSyDeIRToGraph) where

import Data.Function ((&))
import qualified Data.Text as T
import ForSyDeIR
import LSPUri (filePathToUriText)
import SDFSchedule (Schedule (..))
import SKGraphSchema

-- | Convert a ForSyDe IR system into the graph representation understood by
-- KLighD. When schedule information is available, signal labels show computed
-- buffer sizes; otherwise they retain their signal names.
forSyDeIRToGraph :: FilePath -> IRSystem -> Maybe Schedule -> GraphElement
forSyDeIRToGraph filename (IRSystem (inputs, outputs) actors signals _) sched = graph
  where
    -- \| Create a port with a label for signal rate
    createPort parent rends props (n, _) =
      KPort
        { children = [],
          renderings = rends,
          properties = props,
          gid = pid
        }
      where
        pid = parent <> "$P$" <> T.show n

    -- \| Create a node based on an IRActor
    createNode = \case
      IRActor name _ _ _ ->
        createNode'
          name
          (Just $ T.show name)
          [KRoundedRectangle [KBackgroundColor 160 160 240] 4 4]
          [ NodeLabelsPlacement [1, 4, 6],
            NodeSizeConstraints [0, 1, 2, 3],
            NodeSizeMinimum [64, 64],
            PortConstraints 2
          ]
      IRDelay name tokens _ ->
        createNode'
          name
          (Just $ T.show $ length tokens)
          [KEllipse [KBackgroundColor 0 0 0]]
          [ NodeLabelsPlacement [1, 3, 7],
            NodeSizeConstraints [3],
            NodeSizeMinimum [12, 12],
            PortConstraints 2
          ]

    -- \| Find all signals which the process is the source of
    findOutputSignals sigs proc =
      foldr f [] sigs
      where
        f signal acc =
          let IRSignal name (sourceProcess, rate) _ = signal
           in if sourceProcess == proc then (name, rate) : acc else acc

    -- \| Find all signals which the process is the target of
    findInputSignals sigs proc =
      foldr f [] sigs
      where
        f signal acc =
          let IRSignal name _ (targetProcess, rate) = signal
           in if targetProcess == proc then (name, rate) : acc else acc

    -- \| Helper to create signal label, including buffersize if schedule is known
    signalLabel parent name = labels
      where
        labelId = parent <> "$L$" <> T.show name
        nameLabel = [KLabel {gid = labelId, label = T.show name, properties = [NodeLabelsPlacement [1, 5, 7]]}]
        labels = case sched of
          Just (Schedule (_, buffers, _)) ->
            case lookup name buffers of
              Just size -> [KLabel {gid = labelId, label = "|" <> T.show name <> "| = " <> T.show size, properties = [NodeLabelsPlacement [1, 5, 7]]}]
              Nothing -> []
          Nothing -> nameLabel

    -- \| Helper for createNode and global inputs / outputs
    createNode' name nodeLabel nodeRenderings nodeProperties = node
      where
        nodeId = "$root$N$" <> T.show name
        inputSignals = findInputSignals signals name
        outputSignals = findOutputSignals signals name
        inputPortRendering = if delayId name actors then [] else [KText "▶" []]
        inputPorts = map (createPort nodeId inputPortRendering [PortSide 4]) inputSignals
        outputPorts = map (createPort nodeId [] [PortSide 2]) outputSignals
        signalLabels =
          if delayId name actors
            then map (\(signalId, _) -> signalLabel nodeId signalId) inputSignals & mconcat
            else []
        nodeLabels = maybe [] (\labelText -> [KLabel {gid = nodeId <> "$L$" <> T.show name, label = labelText, properties = []}]) nodeLabel
        node =
          KNode
            { gid = nodeId,
              children = inputPorts <> outputPorts <> signalLabels <> nodeLabels,
              renderings = nodeRenderings,
              properties = nodeProperties
            }

    -- \| Helper to check if an id belongs to a delay
    delayId delayName =
      any
        ( \case
            IRDelay actorId _ _ -> actorId == delayName
            _ -> False
        )

    -- \| Create an edge from an IRSignal, depends on port id
    createEdge (IRSignal name (sourceName, sourceRate) (targetName, targetRate)) = edge
      where
        sourcePort = "$root$N$" <> T.show sourceName <> "$P$" <> T.show name
        targetPort = "$root$N$" <> T.show targetName <> "$P$" <> T.show name
        edgeId = sourcePort <> "$E$" <> T.show name
        signalLabelId = edgeId <> "$L$" <> T.show name
        targetRateLabel = [KLabel {gid = signalLabelId <> "$" <> T.show targetName, label = T.show targetRate, properties = [EdgeLabelsPlacement 1]}]
        sourceRateLabel = [KLabel {gid = signalLabelId <> "$" <> T.show sourceName, label = T.show sourceRate, properties = [EdgeLabelsPlacement 2]}]
        (sourceLabels, targetLabels, bufferLabels) = case (delayId sourceName actors, delayId targetName actors) of
          (True, True) -> ([], [], [])
          (True, False) -> ([], targetRateLabel, [])
          (False, True) -> (sourceRateLabel, [], [])
          (False, False) -> (sourceRateLabel, targetRateLabel, signalLabel edgeId name)
        edgeLabels = case name of
          _ | name == sourceName -> targetLabels
          _ | name == targetName -> sourceLabels
          _ -> bufferLabels <> sourceLabels <> targetLabels
        edge =
          KEdge
            { gid = edgeId,
              children = edgeLabels,
              renderings = [KRoundedBendsPolyline [] 4],
              properties = [],
              source = sourcePort,
              target = targetPort
            }

    nodes =
      map createNode actors
        <> map (\name -> createNode' name (Just $ T.show name) [] [LayerConstraint 2]) inputs
        <> map (\name -> createNode' name (Just $ T.show name) [] [LayerConstraint 4]) outputs
    edges = map createEdge signals
    graph =
      KGraph
        { gid = filePathToUriText filename,
          child =
            KNode
              { gid = "$root",
                children = nodes ++ edges,
                properties = [],
                renderings = []
              },
          properties = []
        }
