{-# OPTIONS_GHC -Wno-tabs #-}

module Prime where

import System.Random
import System.Environment
import Control.Monad

ps = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]

bitsToInteger :: [Integer] -> Integer
bitsToInteger bs = bitsToInteger_ bs 0

bitsToInteger_ :: [Integer] -> Integer -> Integer
bitsToInteger_ [] n = n
bitsToInteger_ (b:bs) n = bitsToInteger_ bs (n*2 + b)

rem2 :: Integer -> (Integer, Integer)
rem2 x = rem2_ x 0

rem2_ :: Integer -> Integer -> (Integer, Integer)
rem2_ x twos
	| x `mod` 2 == 0 = rem2_ (x `div` 2) $! twos+1
	| otherwise = (twos, x)

takeA :: Integer -> Integer -> IO (Bool, Integer)
takeA n t = do
	g <- newStdGen
	let a = head (randomRs (0, n-1) g)
	if ((gcd a n) == 1)
	then do
		let z = (a^t) `mod` n
		if (z == 1) || (z == (n-1))
		then takeA n t
		else return (True, a)
	else return (False, 0)

prime :: Integer -> IO Bool
prime n = do
	if elem n ps
	then return True
	else prime_ n 32 

prime_ :: Integer -> Int -> IO Bool
prime_ n iter = do
	let (s,t) = rem2 $! n-1
	z <- takeA n t
	if not . fst $ z
	then return False
	else do
		let a = snd z
		if ((filter (\x -> x == n-1)
			. map (\x -> a^((2^x)*t)) $ [0..(s-1)]) == [])
		then
			if iter == 0
			then return True
			else do
				r <- (prime_ n $! iter-1)
				return r
		else return False

gen :: Int -> IO (Integer, Integer)
gen bits = gen_ bits 1

gen_ :: Int -> Integer -> IO (Integer, Integer)
gen_ bits iter
	| iter < 100 = do
		g <- newStdGen
		let n = bitsToInteger
			([1] ++ (take (bits-2) (randomRs (0, 1) g)) ++ [1])
		ispr <- prime n
		if ispr
		then return (n, iter)
		else do
			gen_ bits $! iter+1
	| otherwise = return (2, 0)

{-main :: IO ()
main = do
	argv <- getArgs
	when (argv == []) (error "Number of bits expected as an argument.")
	let bs = read (argv !! 0) :: Int
	(n,i) <- gen bs
	putStrLn $ "Number: " ++ (show n)
	putStrLn $ "Iterations: " ++ (show i)-}
