module SDF_example_036 where

import ForSyDe.Shallow

-- System Netlist: 8-Channel Parallel Filterbank
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    (s_L, s_R) = a_split_main s_in
    (c1, c2, c3, c4) = a_split_L s_L
    (c5, c6, c7, c8) = a_split_R s_R

    c1_a = p_f1 c1
    c1_b = p_g1 c1_a

    c2_a = p_f2 c2
    c2_b = p_g2 c2_a

    c3_a = p_f3 c3
    c3_b = p_g3 c3_a

    c4_a = p_f4 c4
    c4_b = p_g4 c4_a

    c5_a = p_f5 c5
    c5_b = p_g5 c5_a

    c6_a = p_f6 c6
    c6_b = p_g6 c6_a

    c7_a = p_f7 c7
    c7_b = p_g7 c7_a

    c8_a = p_f8 c8
    c8_b = p_g8 c8_a

    m_L = a_merge_L c1_b c2_b c3_b c4_b
    m_R = a_merge_R c5_b c6_b c7_b c8_b
    s_out = a_out m_L m_R

-- Process Specifications
a_split_main :: Signal Int -> (Signal Int, Signal Int)
a_split_main s = actor12SDF 2 (1, 1) f_split2 s

a_split_L :: Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
a_split_L s = actor14SDF 1 (1, 1, 1, 1) f_split4 s

a_split_R :: Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
a_split_R s = actor14SDF 1 (1, 1, 1, 1) f_split4 s

p_f1 :: Signal Int -> Signal Int
p_f1 s = actor11SDF 1 1 f_f1 s

p_g1 :: Signal Int -> Signal Int
p_g1 s = actor11SDF 1 1 f_g1 s

p_f2 :: Signal Int -> Signal Int
p_f2 s = actor11SDF 1 1 f_f2 s

p_g2 :: Signal Int -> Signal Int
p_g2 s = actor11SDF 1 1 f_g2 s

p_f3 :: Signal Int -> Signal Int
p_f3 s = actor11SDF 1 1 f_f3 s

p_g3 :: Signal Int -> Signal Int
p_g3 s = actor11SDF 1 1 f_g3 s

p_f4 :: Signal Int -> Signal Int
p_f4 s = actor11SDF 1 1 f_f4 s

p_g4 :: Signal Int -> Signal Int
p_g4 s = actor11SDF 1 1 f_g4 s

p_f5 :: Signal Int -> Signal Int
p_f5 s = actor11SDF 1 1 f_f5 s

p_g5 :: Signal Int -> Signal Int
p_g5 s = actor11SDF 1 1 f_g5 s

p_f6 :: Signal Int -> Signal Int
p_f6 s = actor11SDF 1 1 f_f6 s

p_g6 :: Signal Int -> Signal Int
p_g6 s = actor11SDF 1 1 f_g6 s

p_f7 :: Signal Int -> Signal Int
p_f7 s = actor11SDF 1 1 f_f7 s

p_g7 :: Signal Int -> Signal Int
p_g7 s = actor11SDF 1 1 f_g7 s

p_f8 :: Signal Int -> Signal Int
p_f8 s = actor11SDF 1 1 f_f8 s

p_g8 :: Signal Int -> Signal Int
p_g8 s = actor11SDF 1 1 f_g8 s

a_merge_L :: Signal Int -> Signal Int -> Signal Int -> Signal Int -> Signal Int
a_merge_L s1 s2 s3 s4 = actor41SDF (1, 1, 1, 1) 1 f_merge4 s1 s2 s3 s4

a_merge_R :: Signal Int -> Signal Int -> Signal Int -> Signal Int -> Signal Int
a_merge_R s1 s2 s3 s4 = actor41SDF (1, 1, 1, 1) 1 f_merge4 s1 s2 s3 s4

a_out :: Signal Int -> Signal Int -> Signal Int
a_out s1 s2 = actor21SDF (1, 1) 1 f_merge2 s1 s2

-- Function Definitions
f_split2 :: [Int] -> ([Int], [Int])
f_split2 [x, y] = ([x], [y])

f_split4 :: [Int] -> ([Int], [Int], [Int], [Int])
f_split4 [x] = ([x], [x + 1], [x + 2], [x + 3])

f_f1 :: [Int] -> [Int]
f_f1 [x] = [x * 2 + 1]

f_g1 :: [Int] -> [Int]
f_g1 [x] = [x + 10]

f_f2 :: [Int] -> [Int]
f_f2 [x] = [x * 3 + 2]

f_g2 :: [Int] -> [Int]
f_g2 [x] = [x + 20]

f_f3 :: [Int] -> [Int]
f_f3 [x] = [x * 4 + 3]

f_g3 :: [Int] -> [Int]
f_g3 [x] = [x + 30]

f_f4 :: [Int] -> [Int]
f_f4 [x] = [x * 5 + 4]

f_g4 :: [Int] -> [Int]
f_g4 [x] = [x + 40]

f_f5 :: [Int] -> [Int]
f_f5 [x] = [x * 6 + 5]

f_g5 :: [Int] -> [Int]
f_g5 [x] = [x + 50]

f_f6 :: [Int] -> [Int]
f_f6 [x] = [x * 7 + 6]

f_g6 :: [Int] -> [Int]
f_g6 [x] = [x + 60]

f_f7 :: [Int] -> [Int]
f_f7 [x] = [x * 8 + 7]

f_g7 :: [Int] -> [Int]
f_g7 [x] = [x + 70]

f_f8 :: [Int] -> [Int]
f_f8 [x] = [x * 9 + 8]

f_g8 :: [Int] -> [Int]
f_g8 [x] = [x + 80]

f_merge4 :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
f_merge4 [a] [b] [c] [d] = [a + b + c + d]

f_merge2 :: [Int] -> [Int] -> [Int]
f_merge2 [a] [b] = [a + b]

main :: IO ()
main =
  getLine >>= \line ->
    putStrLn . unwords . map show . fromSignal . system . signal . map read . words $ line
