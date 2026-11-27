module SDF_example_relational_case where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Bool
system s_in = s_out
  where
    s_out = a_a s_in

-- Process specification
a_a :: Signal Int -> Signal Bool
a_a s_in = actor11SDF 1 1 f_1 s_in

-- Function definition
f_1 :: [Int] -> [Bool]
f_1 [x] = case x == 1 of
    True -> [False]
    False -> [True]