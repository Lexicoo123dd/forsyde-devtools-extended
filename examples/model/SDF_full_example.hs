module SDF_full_example where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Float
system s_in = s_out
  where
    (s_1, s_3) = a_adder s_in s_2_delayed
    s_2_delayed = d_1 s_1
    s_out = a_average s_3

-- Process specifications

-- Adder: consumes 1 token from each input,
-- produces 1 token.
a_adder :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_adder s_in s_previous =
  actor22SDF (1, 1) (1, 1) f_adder s_in s_previous

-- Average: consumes 3 tokens,
-- produces 1 token.
a_average :: Signal Int -> Signal Float
a_average s_in =
  actor11SDF 3 1 f_average s_in

-- Function definitions

-- Adds current input and previous result.
f_adder :: [Int] -> [Int] -> ([Int], [Int])
f_adder [x] [y] = ([x + y], [x + y])

f_average :: [Int] -> [Float]
f_average [x, y, z] =
  [fromIntegral (x + y + z) / 3.0]

d_1 s = delaySDF [0] s

main :: IO ()
main =
  getLine >>= \line ->
    putStrLn . unwords . map show . fromSignal . system . signal . map read . words $ line
