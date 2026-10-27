module SDF_example_037 where

import ForSyDe.Shallow

-- System Netlist: Deep 8-Channel Multi-Stage DSP Pipeline
-- 8 Parallel Frequency Channels x 4 Processing Stages per Channel
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    (s_L, s_R) = a_split_main s_in
    (c1, c2, c3, c4) = a_split_L s_L
    (c5, c6, c7, c8) = a_split_R s_R

    -- Channel 1 (4 stages)
    c1_1 = p_s1_c1 c1
    c1_2 = p_s2_c1 c1_1
    c1_3 = p_s3_c1 c1_2
    c1_4 = p_s4_c1 c1_3

    -- Channel 2 (4 stages)
    c2_1 = p_s1_c2 c2
    c2_2 = p_s2_c2 c2_1
    c2_3 = p_s3_c2 c2_2
    c2_4 = p_s4_c2 c2_3

    -- Channel 3 (4 stages)
    c3_1 = p_s1_c3 c3
    c3_2 = p_s2_c3 c3_1
    c3_3 = p_s3_c3 c3_2
    c3_4 = p_s4_c3 c3_3

    -- Channel 4 (4 stages)
    c4_1 = p_s1_c4 c4
    c4_2 = p_s2_c4 c4_1
    c4_3 = p_s3_c4 c4_2
    c4_4 = p_s4_c4 c4_3

    -- Channel 5 (4 stages)
    c5_1 = p_s1_c5 c5
    c5_2 = p_s2_c5 c5_1
    c5_3 = p_s3_c5 c5_2
    c5_4 = p_s4_c5 c5_3

    -- Channel 6 (4 stages)
    c6_1 = p_s1_c6 c6
    c6_2 = p_s2_c6 c6_1
    c6_3 = p_s3_c6 c6_2
    c6_4 = p_s4_c6 c6_3

    -- Channel 7 (4 stages)
    c7_1 = p_s1_c7 c7
    c7_2 = p_s2_c7 c7_1
    c7_3 = p_s3_c7 c7_2
    c7_4 = p_s4_c7 c7_3

    -- Channel 8 (4 stages)
    c8_1 = p_s1_c8 c8
    c8_2 = p_s2_c8 c8_1
    c8_3 = p_s3_c8 c8_2
    c8_4 = p_s4_c8 c8_3

    m_L = a_merge_L c1_4 c2_4 c3_4 c4_4
    m_R = a_merge_R c5_4 c6_4 c7_4 c8_4
    s_out = a_out m_L m_R

-- Process Specifications
a_split_main :: Signal Int -> (Signal Int, Signal Int)
a_split_main s = actor12SDF 2 (1, 1) f_split2 s

a_split_L :: Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
a_split_L s = actor14SDF 1 (1, 1, 1, 1) f_split4 s

a_split_R :: Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
a_split_R s = actor14SDF 1 (1, 1, 1, 1) f_split4 s

p_s1_c1 :: Signal Int -> Signal Int
p_s1_c1 s = actor11SDF 1 1 f_op s
p_s2_c1 :: Signal Int -> Signal Int
p_s2_c1 s = actor11SDF 1 1 f_op s
p_s3_c1 :: Signal Int -> Signal Int
p_s3_c1 s = actor11SDF 1 1 f_op s
p_s4_c1 :: Signal Int -> Signal Int
p_s4_c1 s = actor11SDF 1 1 f_op s

p_s1_c2 :: Signal Int -> Signal Int
p_s1_c2 s = actor11SDF 1 1 f_op s
p_s2_c2 :: Signal Int -> Signal Int
p_s2_c2 s = actor11SDF 1 1 f_op s
p_s3_c2 :: Signal Int -> Signal Int
p_s3_c2 s = actor11SDF 1 1 f_op s
p_s4_c2 :: Signal Int -> Signal Int
p_s4_c2 s = actor11SDF 1 1 f_op s

p_s1_c3 :: Signal Int -> Signal Int
p_s1_c3 s = actor11SDF 1 1 f_op s
p_s2_c3 :: Signal Int -> Signal Int
p_s2_c3 s = actor11SDF 1 1 f_op s
p_s3_c3 :: Signal Int -> Signal Int
p_s3_c3 s = actor11SDF 1 1 f_op s
p_s4_c3 :: Signal Int -> Signal Int
p_s4_c3 s = actor11SDF 1 1 f_op s

p_s1_c4 :: Signal Int -> Signal Int
p_s1_c4 s = actor11SDF 1 1 f_op s
p_s2_c4 :: Signal Int -> Signal Int
p_s2_c4 s = actor11SDF 1 1 f_op s
p_s3_c4 :: Signal Int -> Signal Int
p_s3_c4 s = actor11SDF 1 1 f_op s
p_s4_c4 :: Signal Int -> Signal Int
p_s4_c4 s = actor11SDF 1 1 f_op s

p_s1_c5 :: Signal Int -> Signal Int
p_s1_c5 s = actor11SDF 1 1 f_op s
p_s2_c5 :: Signal Int -> Signal Int
p_s2_c5 s = actor11SDF 1 1 f_op s
p_s3_c5 :: Signal Int -> Signal Int
p_s3_c5 s = actor11SDF 1 1 f_op s
p_s4_c5 :: Signal Int -> Signal Int
p_s4_c5 s = actor11SDF 1 1 f_op s

p_s1_c6 :: Signal Int -> Signal Int
p_s1_c6 s = actor11SDF 1 1 f_op s
p_s2_c6 :: Signal Int -> Signal Int
p_s2_c6 s = actor11SDF 1 1 f_op s
p_s3_c6 :: Signal Int -> Signal Int
p_s3_c6 s = actor11SDF 1 1 f_op s
p_s4_c6 :: Signal Int -> Signal Int
p_s4_c6 s = actor11SDF 1 1 f_op s

p_s1_c7 :: Signal Int -> Signal Int
p_s1_c7 s = actor11SDF 1 1 f_op s
p_s2_c7 :: Signal Int -> Signal Int
p_s2_c7 s = actor11SDF 1 1 f_op s
p_s3_c7 :: Signal Int -> Signal Int
p_s3_c7 s = actor11SDF 1 1 f_op s
p_s4_c7 :: Signal Int -> Signal Int
p_s4_c7 s = actor11SDF 1 1 f_op s

p_s1_c8 :: Signal Int -> Signal Int
p_s1_c8 s = actor11SDF 1 1 f_op s
p_s2_c8 :: Signal Int -> Signal Int
p_s2_c8 s = actor11SDF 1 1 f_op s
p_s3_c8 :: Signal Int -> Signal Int
p_s3_c8 s = actor11SDF 1 1 f_op s
p_s4_c8 :: Signal Int -> Signal Int
p_s4_c8 s = actor11SDF 1 1 f_op s

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

f_op :: [Int] -> [Int]
f_op [x] = [x * 2 + 1]

f_merge4 :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
f_merge4 [a] [b] [c] [d] = [a + b + c + d]

f_merge2 :: [Int] -> [Int] -> [Int]
f_merge2 [a] [b] = [a + b]

main :: IO ()
main =
  getLine >>= \line ->
    putStrLn . unwords . map show . fromSignal . system . signal . map read . words $ line
