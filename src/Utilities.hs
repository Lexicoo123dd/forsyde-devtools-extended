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

import Compiler.CoreIRToForSyDeIR (translateCoreProgram)
import Compiler.GHCCoreAPI (compileToCoreWithForSyDePath)
import Schedule (Schedule, computeScheduleAndBuffers)
import Compiler.CoreIRToForSyDeIR (translateCoreProgram)
import Data.Data (Data, gmapT)
import Data.Generics (extT)
import GHC
import GHC.Driver.Main
import GHC.Paths (libdir)
import GHC.Plugins hiding (isEmpty)
import GHC.Tc.Types
import Schedule (Schedule, computeScheduleAndBuffers)
import System.FilePath (takeBaseName)

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
