module Random (random, range)
where

-- musl parameters for linear congruential generation.
m, a, c :: Integer
m = 2 ^ 64
a = 6364136223846793005
c = 1

random :: Integer -> Integer
random seed = mod (a * seed + c) m

range :: Fractional a => Integer -> a
range seed = fromIntegral (2 * seed) / 2 ^ 64 - 1
