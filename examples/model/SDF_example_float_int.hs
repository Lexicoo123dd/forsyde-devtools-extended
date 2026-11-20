module SDF_example_float_int where

import ForSyDe.Shallow

-- Netlist
system :: Signal Float -> Signal Int -> (Signal Float, Signal Int)
system s_in_float s_in_int = (s_out_float, s_out_int)
  where
    s_out_float = a_a s_in_float
    s_out_int = a_b s_in_int

-- Process specifications
a_a :: Signal Float -> Signal Float
a_a s = actor11SDF 1 1 addFloat s

a_b :: Signal Int -> Signal Int
a_b s = actor11SDF 1 1 addInt s

-- Function definitions
addFloat :: [Float] -> [Float]
addFloat [x] = [x + 1.0]

addInt :: [Int] -> [Int]
addInt [x] = [x + 1]
