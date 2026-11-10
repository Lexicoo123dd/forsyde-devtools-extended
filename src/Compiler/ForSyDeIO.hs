module Compiler.ForSyDeIO where

import Compiler.ForSyDeIR
import Data.List (intercalate)
import Text.Printf (printf)

prettyConstructors :: IRConstructor -> String -> String
prettyConstructors (IRActor actorId _type _functionId (inputSignals, outputSignals)) indent =
  printf "%svertex \"%s\"\n\
          \%s[moc::sdf::SDFActor]\n\
          \%s(combFunctions, %s, %s)\n\
          \%s{\n\
          \%s}"
  indent
  (show actorId)
  indent
  indent
  (intercalate ", " (map show inputSignals))
  (intercalate ", " (map show outputSignals))
  indent
  indent

prettyIOGlobal :: IRSystem -> String
prettyIOGlobal (IRSystem (_globalinputs, _globaloutputs) constructors _signals _functions) =
  let indent = "  " in
  printf "systemgraph {\n%s\n}"
  (intercalate "\n" (map (\c -> prettyConstructors c indent) constructors)) 
