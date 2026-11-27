module SDF_example_if_bool where

import ForSyDe.Shallow

-- Netlist
system :: Signal Bool -> Signal Bool
system s_in = s_out
  where
    s_out = a_a s_in

-- Process specification
a_a :: Signal Bool -> Signal Bool
a_a s_in = actor11SDF 1 1 f_1 s_in

-- Function definition
f_1 :: [Bool] -> [Bool]
f_1 [x] = case x of
  True -> [False]
  False -> [True]