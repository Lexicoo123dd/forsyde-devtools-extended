module Utilities
  ( scheduleAndBuffer,
    Stack,
    emptyStack,
    push,
    pop,
    peek,
    isEmpty,
    stackToList,
  )
where

import CoreIRToForSyDeIR (translateCoreProgram)
import SDFSchedule (Schedule, computeScheduleAndBuffers)
import GHCCoreAPI (compileToCoreWithForSyDePath)

newtype Stack a = Stack [a] deriving (Show, Eq)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : _)) = Just x

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False

stackToList :: Stack a -> [a]
stackToList (Stack list) = list

-- | Utility function to obtain output of SDF Scheduler. Meant for testing to be
-- used in a repl. Currently unused.
scheduleAndBuffer :: FilePath -> IO Schedule
scheduleAndBuffer filePath = do
  (core, dflags) <- compileToCoreWithForSyDePath Nothing filePath
  case translateCoreProgram dflags core of
    Left e -> error e
    Right (forsydeIR, _lookupSignals) ->
      pure $ case computeScheduleAndBuffers forsydeIR of
        Left e -> error e
        Right v -> v
