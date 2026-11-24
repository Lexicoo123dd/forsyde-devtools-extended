{- |
Module      : Compiler.SchedulerMain
Description : Unified SDF scheduling interface and top-level coordinator.

Architecture:
  - 'Compiler.Scheduler.Common'  : Shared graph structures, matrix mathematics, and buffer simulation.
  - 'Compiler.Scheduler.Greedy'  : Single-core greedy PASS scheduler.
  - 'Compiler.SchedulerMain'         : Top-level dispatcher coordinating analysis and strategy execution.
-}

module Compiler.SchedulerMain
  ( -- * Top-Level Scheduling Entrypoints
    generateSchedule,
    prettySchedule,
  )
where

import Compiler.ForSyDeIR (IRSystem)
import Compiler.Scheduler.Common
import Compiler.Scheduler.Greedy

-- | Generates a unified Schedule given a scheduling strategy and an IRSystem.
generateSchedule :: SchedulingStrategy -> IRSystem -> Either String Schedule
generateSchedule strategy irSystem = case strategy of
  Greedy -> generateGreedySchedule irSystem

-- | Pretty-prints a schedule according to its strategy and the IRSystem.
prettySchedule :: IRSystem -> Schedule -> String
prettySchedule irSystem sched = case schedStrategy sched of
  Greedy -> prettyGreedySchedule irSystem sched
