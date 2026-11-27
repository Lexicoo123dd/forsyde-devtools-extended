module SDF_example_if_float where

import ForSyDe.Shallow

-- Netlist
system :: Signal Float -> Signal Float
system s_in = s_out
  where
    s_out = a_a s_in

-- Process specification
a_a :: Signal Float -> Signal Float
a_a s_in = actor11SDF 1 1 f_1 s_in

-- Function definition
f_1 :: [Float] -> [Float]
f_1 [x] = case x of
    1.0 -> [0.0]
    _ -> [1.0]