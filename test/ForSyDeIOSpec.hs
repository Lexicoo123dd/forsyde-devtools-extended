module ForSyDeIOSpec (spec) where

import Compiler.CoreIRToForSyDeIR (translateCoreProgram)
import Compiler.ForSyDeIO
import Compiler.ForSyDeIR
import Compiler.GHCCoreAPI (compileToCoreWithForSyDePath)
import GHC (DynFlags, runGhc, getSessionDynFlags)
import GHC.Paths (libdir)
import Test.Hspec

singleActorSystem :: IRSystem
singleActorSystem =
  IRSystem
    ([IRString "s_in"], [IRString "s_out"])
    [ IRActor (IRString "actor_1") Actor11 (IRString "f_1") ([IRString "s_in"], [IRString "s_out"])
    ]
    [ IRSignal (IRString "s_in") (IRString "s_in", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_out") (IRString "actor_1", 1) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "f_1") Nothing
    ]

twoActorsSystem :: IRSystem
twoActorsSystem =
  IRSystem
    ([IRString "s_in"], [IRString "s_out"])
    [ IRActor (IRString "actor_1") Actor11 (IRString "f_1") ([IRString "s_in"], [IRString "s_1"]),
      IRActor (IRString "actor_2") Actor11 (IRString "f_2") ([IRString "s_1"], [IRString "s_out"])
    ]
    [ IRSignal (IRString "s_in") (IRString "s_in", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_1") (IRString "actor_1", 1) (IRString "actor_2", 1),
      IRSignal (IRString "s_out") (IRString "actor_2", 1) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "f_1") Nothing,
      IRFunction (IRString "f_2") Nothing
    ]

delaysSystem :: IRSystem
delaysSystem =
  IRSystem
    ([IRString "s_in"], [IRString "s_out"])
    [ IRActor (IRString "actor_1") Actor11 (IRString "f_1") ([IRString "s_in"], [IRString "s_1"]),
      IRActor (IRString "actor_2") Actor11 (IRString "f_2") ([IRString "s_2"], [IRString "s_out"]),
      IRDelay (IRString "delay_1") [0] (IRString "s_1", IRString "s_2")
    ]
    [ IRSignal (IRString "s_in") (IRString "s_in", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_1") (IRString "actor_1", 1) (IRString "delay_1", 1),
      IRSignal (IRString "s_2") (IRString "delay_1", 1) (IRString "actor_2", 1),
      IRSignal (IRString "s_out") (IRString "actor_2", 1) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "f_1") Nothing,
      IRFunction (IRString "f_2") Nothing
    ]

loopedSystem :: IRSystem
loopedSystem =
  IRSystem
    ([IRString "s_in"], [IRString "s_out"])
    [ IRActor (IRString "actor_1") Actor11 (IRString "f_1") ([IRString "s_in", IRString "s_loop2"], [IRString "s_1", IRString "s_loop"]),
      IRActor (IRString "actor_2") Actor11 (IRString "f_2") ([IRString "s_1"], [IRString "s_out"]),
      IRDelay (IRString "delay_1") [0] (IRString "s_loop", IRString "s_loop2")
    ]
    [ IRSignal (IRString "s_in") (IRString "s_in", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_1") (IRString "actor_1", 1) (IRString "actor_2", 1),
      IRSignal (IRString "s_loop") (IRString "actor_1", 1) (IRString "delay_1", 1),
      IRSignal (IRString "s_loop2") (IRString "delay_1", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_out") (IRString "actor_2", 1) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "f_1") Nothing,
      IRFunction (IRString "f_2") Nothing
    ]


customDflags :: IO DynFlags
customDflags = runGhc (Just libdir) getSessionDynFlags

spec :: SpecWith ()
spec = do
  describe "ForSyDe IO output .fiodl" $ do
    it "Empty system" $ do
      dflags <- customDflags
      emptySystemString <- readFile "examples/test/Empty.fiodl"
      prettyForSyDeIO dflags (IRSystem ([], []) [] [] [])
        `shouldBe` emptySystemString
    -- | Note that a system with a single actor gives an error in idesyde,
    -- as it crashes if there are no edges
    it "System with single actor" $ do
      dflags <- customDflags
      singleActorSystemString <- readFile "examples/test/SingleActor.fiodl"
      prettyForSyDeIO dflags singleActorSystem
        `shouldBe` singleActorSystemString
    it "System with two actors" $ do
      dflags <- customDflags
      twoActorsSystemString <- readFile "examples/test/TwoActors.fiodl"
      prettyForSyDeIO dflags twoActorsSystem
        `shouldBe` twoActorsSystemString
    it "System with delays" $ do
      dflags <- customDflags
      delaysSystemString <- readFile "examples/test/Delays.fiodl"
      prettyForSyDeIO dflags delaysSystem
        `shouldBe` delaysSystemString
    it "System with loops" $ do
      dflags <- customDflags
      loopedSystemString <- readFile "examples/test/Looped.fiodl"
      prettyForSyDeIO dflags loopedSystem
        `shouldBe` loopedSystemString
    it "System with functions" $ do
      let inputFile = "examples/test/Functions.hs"
      (core, dflags) <- compileToCoreWithForSyDePath Nothing inputFile
      let (functionsSystem, _) = translateCoreHelper dflags core
      functionsSystemString <- readFile "examples/test/Functions.fiodl"
      prettyForSyDeIO dflags functionsSystem
        `shouldBe` functionsSystemString
  where
    translateCoreHelper dflags core = case translateCoreProgram dflags core of
      Left e -> error e
      Right v -> v
