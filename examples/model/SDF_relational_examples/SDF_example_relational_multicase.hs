module SDF_example_relational_multicase where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    s_out = a_a s_in

-- Process specification
a_a :: Signal Int -> Signal Int
a_a s_in = actor11SDF 1 1 f_1 s_in

-- Function definition
f_1 :: [Int] -> [Int]
f_1 [x] = case x == 0 of
  True -> [0]
  False -> case x < 0 of
    True -> [-1]
    False -> [1]