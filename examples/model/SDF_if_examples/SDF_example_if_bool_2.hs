module SDF_example_if_bool_2 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Bool -> Signal Bool -> Signal Bool
system s_in1 s_in2 = s_out
  where
    s_out = a_a s_in1 s_in2

-- Process specification
a_a :: Signal Bool -> Signal Bool -> Signal Bool
a_a s_in1 s_in2 = actor21SDF (1,1) 1 f_1 s_in1 s_in2

-- Function definition
f_1 :: [Bool] -> [Bool] -> [Bool]
f_1 [x] [y] = if x == y then [True] else [False] 