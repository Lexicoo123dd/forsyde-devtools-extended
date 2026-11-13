{- |
Module      : Schedule
Description : Unified SDF scheduling interface and top-level coordinator.

Architecture:
  - 'Schedule.Common'  : Shared graph structures, matrix mathematics, and buffer simulation.
  - 'Schedule.Greedy'  : Single-core greedy PASS scheduler.
  - 'Schedule.HuLevel' : Multi-core list scheduler using Acyclic Precedence Graphs (APG).
  - 'Schedule'         : Top-level dispatcher coordinating analysis and strategy execution.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Schedule
  ( -- * Top-Level Scheduling Entrypoints
    generateSchedule,
    computeScheduleAndBuffers,
    prettySchedule,

    -- * Sub-module Re-exports
    module Schedule.Common,
    module Schedule.Greedy,
    module Schedule.HuLevel,
  )
where

import ForSyDeIR (IRSystem)
import Schedule.Common
import Schedule.Greedy
import Schedule.HuLevel

-- | Generates a unified Schedule given a scheduling strategy and an IRSystem.
generateSchedule :: SchedulingStrategy -> IRSystem -> Either String Schedule
generateSchedule strategy irSystem = case strategy of
  Greedy -> generateGreedySchedule irSystem
  HuLevel numCores -> generateHuLevelSchedule numCores irSystem

-- | Backwards-compatible alias for the default greedy single-core scheduler.
computeScheduleAndBuffers :: IRSystem -> Either String Schedule
computeScheduleAndBuffers = generateSchedule Greedy

-- | Pretty-prints a schedule according to its strategy and the IRSystem.
prettySchedule :: IRSystem -> Schedule -> String
prettySchedule irSystem sched = case schedStrategy sched of
  Greedy -> prettyGreedySchedule irSystem sched
  HuLevel numCores -> prettyHuLevelSchedule numCores irSystem sched
