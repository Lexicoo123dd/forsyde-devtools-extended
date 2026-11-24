module ExecutionTimeSpec (spec) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "SDF Multi-Core Execution Time & Speedup Benchmark" $ do
  it "achieves parallel speedup close to schedule limits across all example SDF models" $ do
    (code, stdout, stderr) <-
      readProcessWithExitCode
        "python3"
        [ "scripts/benchmark_execution_time.py",
          "--cores",
          "2",
          "--workload",
          "100000",
          "--iterations",
          "50",
          "--repeats",
          "2"
        ]
        ""
    case code of
      ExitSuccess -> pure ()
      ExitFailure c ->
        expectationFailure $
          "Execution time benchmark failed with exit code "
            ++ show c
            ++ ":\n"
            ++ stdout
            ++ "\n"
            ++ stderr
