module Functions where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    s_out = actor_2 s_1
    s_1 = actor_1 s_in

-- Process specifications
actor_1 :: Signal Int -> Signal Int
actor_1 s_in = actor11SDF 1 1 f_1 s_in

actor_2 :: Signal Int -> Signal Int
actor_2 s_in = actor11SDF 1 1 f_2 s_in

-- Function definitions
f_1 :: [Int] -> [Int]
f_1 [x] = [x + 1]

f_2 :: [Int] -> [Int]
f_2 [x] = [x * 2]
