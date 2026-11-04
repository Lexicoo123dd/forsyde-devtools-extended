{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VisualiserKLighDSpec (spec) where

import qualified Data.Text as T
import ForSyDeIR
import LSPUri (filePathToUriText)
import SDFSchedule (Schedule (..))
import SKGraphSchema
import Test.Hspec
import Visualiser.KLighD (forSyDeIRToGraph)

spec :: Spec
spec = describe "KLighD diagram synthesis" $ do
  it "preserves graph identity and actor-input-output-edge ordering" $ do
    -- This synthetic path is a test fixture and is never accessed on disk.
    let path = "D:\\example\\path\\model.hs"
        graph = forSyDeIRToGraph path pipelineModel Nothing
    graphId graph `shouldBe` filePathToUriText path
    map elementId (rootChildren graph)
      `shouldBe` [ "$root$N$a",
                   "$root$N$b",
                   "$root$N$input",
                   "$root$N$output",
                   "$root$N$input$P$s_in$E$s_in",
                   "$root$N$a$P$s_mid$E$s_mid",
                   "$root$N$b$P$s_out$E$s_out"
                 ]

  it "uses signal names before scheduling and buffer sizes afterwards" $ do
    let unscheduledEdge = findById "$root$N$a$P$s_mid$E$s_mid" $ rootChildren $ forSyDeIRToGraph "model.hs" pipelineModel Nothing
        schedule = Schedule ([IRString "a", IRString "b"], [(IRString "s_mid", 2)], [])
        scheduledEdge = findById "$root$N$a$P$s_mid$E$s_mid" $ rootChildren $ forSyDeIRToGraph "model.hs" pipelineModel (Just schedule)
    edgeLabels unscheduledEdge `shouldBe` ["s_mid", "1", "1"]
    edgeLabels scheduledEdge `shouldBe` ["|s_mid| = 2", "1", "1"]

  it "renders delay tokens on the delay node and suppresses adjacent buffer labels" $ do
    let elements = rootChildren $ forSyDeIRToGraph "model.hs" delayModel Nothing
        delayNode = findById "$root$N$delay" elements
        inputEdge = findById "$root$N$a$P$s_delay$E$s_delay" elements
        outputEdge = findById "$root$N$delay$P$s_after$E$s_after" elements
    nodeLabels delayNode `shouldContain` ["s_delay", "2"]
    isDelayRendering delayNode `shouldBe` True
    edgeLabels inputEdge `shouldBe` ["2"]
    edgeLabels outputEdge `shouldBe` ["5"]

-- These fixtures contain only the semantics needed by the diagram synthesis
-- seam, so failures point to graph conversion rather than compiler behavior.
pipelineModel :: IRSystem
pipelineModel =
  IRSystem
    ([IRString "input"], [IRString "output"])
    [ IRActor (IRString "a") Actor11 (IRString "identity") ([IRString "s_in"], [IRString "s_mid"]),
      IRActor (IRString "b") Actor11 (IRString "identity") ([IRString "s_mid"], [IRString "s_out"])
    ]
    [ IRSignal (IRString "s_in") (IRString "input", 1) (IRString "a", 1),
      IRSignal (IRString "s_mid") (IRString "a", 1) (IRString "b", 1),
      IRSignal (IRString "s_out") (IRString "b", 1) (IRString "output", 1)
    ]
    [IRFunction (IRString "identity") Nothing]

delayModel :: IRSystem
delayModel =
  IRSystem
    ([], [])
    [ IRActor (IRString "a") Actor11 (IRString "identity") ([], [IRString "s_delay"]),
      IRDelay (IRString "delay") [0, 0] (IRString "s_delay", IRString "s_after"),
      IRActor (IRString "b") Actor11 (IRString "identity") ([IRString "s_after"], [])
    ]
    [ IRSignal (IRString "s_delay") (IRString "a", 2) (IRString "delay", 3),
      IRSignal (IRString "s_after") (IRString "delay", 4) (IRString "b", 5)
    ]
    [IRFunction (IRString "identity") Nothing]

graphId :: GraphElement -> T.Text
graphId KGraph {gid = identifier} = identifier
graphId _ = error "expected KGraph"

rootChildren :: GraphElement -> [GraphElement]
rootChildren KGraph {child = KNode {children = elements}} = elements
rootChildren _ = error "expected KGraph with a KNode root"

elementId :: GraphElement -> T.Text
elementId = \case
  KLabel {gid = identifier} -> identifier
  KNode {gid = identifier} -> identifier
  KPort {gid = identifier} -> identifier
  KEdge {gid = identifier} -> identifier
  KGraph {gid = identifier} -> identifier

findById :: T.Text -> [GraphElement] -> GraphElement
findById identifier elements =
  case filter ((== identifier) . elementId) elements of
    [element] -> element
    _ -> error $ "expected one graph element with id " ++ T.unpack identifier

edgeLabels :: GraphElement -> [T.Text]
edgeLabels KEdge {children = elements} = directLabels elements
edgeLabels _ = error "expected KEdge"

nodeLabels :: GraphElement -> [T.Text]
nodeLabels KNode {children = elements} = directLabels elements
nodeLabels _ = error "expected KNode"

directLabels :: [GraphElement] -> [T.Text]
directLabels elements = [labelText | KLabel {label = labelText} <- elements]

isDelayRendering :: GraphElement -> Bool
isDelayRendering KNode {renderings = [KEllipse _]} = True
isDelayRendering _ = False
