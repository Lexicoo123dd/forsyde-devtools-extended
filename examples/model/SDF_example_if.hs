module SDF_example_if where

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
f_1 [x] = case x of
  0 -> [0]
  _ -> [1]