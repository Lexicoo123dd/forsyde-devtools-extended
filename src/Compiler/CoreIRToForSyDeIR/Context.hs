-- | Defines the data structures used to maintain state during translation.
--
-- The TranslationContext stores the information accumulated while
-- translating Core into ForSyDe IR, including constructors, signals,
-- functions, system inputs and outputs, and binder associations.
--
-- Should handle:
--   * Defining TranslationContext and its fields.
--   * Defining Binder and other context-related data types.
--   * Initialising an empty TranslationContext.
--
-- Should not handle the actual translation of Core expressions or
-- the construction of signals and process constructors.
module Compiler.CoreIRToForSyDeIR.Context where

import Compiler.ForSyDeIR
import GHC hiding (targetId)

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of Core IR to ForSyDe IR.
-- The `TranslationContext` is also used to store constructors, signals,
-- and functions whilst they are being built during the translation.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    constructors :: [(IRId, IRConstructor)], -- Associated list of pcIds and IRConstructors
    signals :: [(IRId, IRSignal)], -- Associated list of signalIds and IRSignals
    functions :: [(IRId, IRFunction)], -- Associated list of functionIds and IRFunctions
    systemInputs :: [IRId], -- List of global inputs to netlist
    systemOutputs :: [IRId], -- List of global outputs to netlist
    nameCounter :: Int, -- Counter used for naming signals
    pcRates :: [(IRId, ([Int], [Int]))], -- Associated list of pcIds and input and ouput rates
    binders :: [(IRId, Binder)] -- Associated list of binderIds and Binders
  }

-- | `Binder` is a data type used to represent what a `CoreBndr` is associated
-- to. Can either directly represent a process constructor through `PcId` or
-- indirectly represent a process constructor through `Binding` with a
-- bindingId and index. Note that in the case of a `Binding` the bindingId is
-- associated with a multi-output process constructor and the index identifies
-- a specific output.
data Binder
  = PcId IRId
  | Binding IRId Int
  deriving (Show)

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags,
      constructors = [],
      signals = [],
      functions = [],
      systemInputs = [],
      systemOutputs = [],
      nameCounter = 1,
      pcRates = [],
      binders = []
    }
