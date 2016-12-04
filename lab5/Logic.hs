{-# OPTIONS_GHC -Wno-tabs #-}

module Logic where

import Data.List
import Data.List.Split

n = 15.0 :: Float
scr = 5.991 :: Float

{- run like `gen a c size [x0]` -}
{- 16 4 are nice! -}
gen :: Integer -> Integer -> Integer-> [Integer] -> [Integer]
{- tail will remove x0 -}
gen _ _ 0 list = tail list
gen a c cnt list = gen a c (cnt-1) $ list ++ [((a * (last list) + c) `mod` (floor n))]

period :: [Integer] -> Int
period xs = length $ nub xs

count :: [Integer] -> [Float]
count list = count_ [0,0,0] list

count_ :: [Float] -> [Integer] -> [Float]
count_ ns [] = ns
count_ (n1:n2:n3:[]) (x:xs)
	| x >= 0 && x < 5 = count_ ((n1+1):n2:[n3]) xs
	| x >= 5 && x < 10 = count_ (n1:(n2+1):[n3]) xs
	| x >= 10 && x < 15 = count_ (n1:n2:[n3+1]) xs
	| otherwise = count_ (n1:n2:[n3]) xs

{- WARN: hardcoded 1/3. Use only with n `mod` 3 = 0. -}
pearson :: [Integer] -> (Float, Bool)
pearson list = (s, if s < scr then True else False)
	where s = n * (sum (map (\x -> ((x/n - 1/3)**2)*3) (count list)))
