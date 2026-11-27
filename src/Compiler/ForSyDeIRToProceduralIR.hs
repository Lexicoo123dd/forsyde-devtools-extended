module Compiler.ForSyDeIRToProceduralIR
  ( generateInputHeader,
    translateIRSystemToProgram,
  )
where

import ArgumentsMain (InputType (..), Runs (..), Target (..))
import Compiler.ForSyDeIR
import Compiler.ForSyDeIRToProceduralIR.Common
import Compiler.ForSyDeIRToProceduralIR.MultiCore (translateMultiCoreProgram)
import Compiler.ForSyDeIRToProceduralIR.SingleCore (translateSingleCoreProgram)
import Compiler.ProceduralIR
import Compiler.Scheduler.Common (Schedule (..), recordToFlatSchedule)
import Data.List (find)
import GHC hiding (Target, targetId)

initialTranslationContext :: DynFlags -> InputType -> Runs -> [(IRId, IRSignal)] -> [(IRId, Int)] -> [IRId] -> [IRId] -> [(IRId, IRId)] -> TranslationContext
initialTranslationContext dflags input r lookupSignalList bList inputList outputList delayBufferList =
  TranslationContext
    { flags = dflags,
      inputType = input,
      runs = r,
      lookupSignals = lookupSignalList,
      actors = [],
      signals = [],
      ioTokens = [],
      initBuffers = [],
      freeBuffers = [],
      schedulerBufferList = bList,
      systemInputs = inputList,
      systemOutputs = outputList,
      delayBuffers = delayBufferList,
      initDelay = [],
      initFunctions = [],
      functions = []
    }

-- | Generates an `input.h` header with #define NUM_ITERATIONS, consumption rates,
-- and placeholder static arrays for each system input.
generateInputHeader :: IRSystem -> Schedule -> String
generateInputHeader (IRSystem (inputList, _) _ irSignals _) schedule =
  let flatSched = recordToFlatSchedule schedule
      headerGuard = "#ifndef INPUT_H\n#define INPUT_H\n\n#define NUM_ITERATIONS 1\n#define iteration_max NUM_ITERATIONS\n\n"
      footerGuard = "#endif // INPUT_H\n"

      genInputDef sigId =
        let (tgtActorId, tgtRate) = case find (\(IRSignal id' _ _) -> id' == sigId) irSignals of
              Just (IRSignal _ _ (dstId, rate)) -> (dstId, rate)
              Nothing -> (sigId, 1)
            reps = length (filter (== tgtActorId) flatSched)
            totalRate = max 1 (reps * tgtRate)
            rateDef = "#define RATE_" ++ show sigId ++ " " ++ show totalRate ++ "\n"
            arrayDecl = "static const token input_" ++ show sigId ++ "[NUM_ITERATIONS * RATE_" ++ show sigId ++ "] = { 0 };\n"
         in rateDef ++ arrayDecl ++ "\n"

      body =
        if null inputList
          then ""
          else "/* Input consumption rates per schedule period and placeholder input arrays */\n"
                 ++ concatMap genInputDef inputList
   in headerGuard ++ body ++ footerGuard

-- | Translates a ForSyDe IR `IRSystem` into a Procedural IR `Program`. Requires
-- the schedule, buffer, and delay buffer lists from the SDF scheduler as an
-- input. Also requires an associated list of signals for lookups.
translateIRSystemToProgram :: DynFlags -> Schedule -> [(IRId, IRSignal)] -> Target -> InputType -> Runs -> IRSystem -> Program
translateIRSystemToProgram dflags schedule lookupSignalList target input r irSystem@(IRSystem (inputList, outputList) constructors _signalList functionList) = do
  let schedMatrix = schedPerCore schedule
      bufferList = schedBufferSizes schedule
      delayBufferList = schedDelayBuffers schedule
      initialContext = initialTranslationContext dflags input r lookupSignalList bufferList inputList outputList delayBufferList
  if length schedMatrix <= 1
    then
      translateSingleCoreProgram schedule initialContext irSystem
    else
      translateMultiCoreProgram schedule initialContext target constructors functionList inputList outputList irSystem
