module ComparisonSpec (spec) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "SDF Schedule Equivalence (Seq vs Multi-Core)" $ do
  it "produces identical outputs across example SDF models" $ do
    (code, stdout, stderr) <- readProcessWithExitCode "python3" ["scripts/compare_schedules.py"] ""
    case code of
      ExitSuccess -> pure ()
      ExitFailure c ->
        expectationFailure $
          "Comparison test failed with exit code "
            ++ show c
            ++ ":\n"
            ++ stdout
            ++ "\n"
            ++ stderr
