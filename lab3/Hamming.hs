{-# OPTIONS_GHC -Wno-tabs #-}

module Hamming where

import Data.Matrix
import Data.Char
{-import Control.Monad-}

hm :: Matrix Int
hm = fromLists
	[[0,0,0,0,1,1,0,0,0]
	,[0,1,1,1,1,0,1,0,0]
	,[1,0,1,1,1,0,0,1,0]
	,[1,1,0,1,0,0,0,0,1]]
	{-[[0,0,0,0,0,0,0,1,1]-}
	{-,[0,0,0,1,1,1,1,0,1]-}
	{-,[0,1,1,0,0,1,1,0,1]-}
	{-,[1,0,1,0,1,0,1,0,0]]-}

gm :: Matrix Int
gm = fromLists
	[[1,0,0,0,0,0,0,1,1]
	,[0,1,0,0,0,0,1,0,1]
	,[0,0,1,0,0,0,1,1,0]
	,[0,0,0,1,0,0,1,1,1]
	,[0,0,0,0,1,1,1,1,0]]

zeroSyn = -2

{- finds number of element in list -}
findElem :: Eq a => a -> [a] -> Int
findElem a as = if a `elem` as
		then fst . head . filter (\(_,x) -> x == a) . zip [0..] $ as
		else (-1)

{- gives 4-digits syndrome from 9-digits word -}
decode :: [Int] -> [Int]
decode w = map (\x -> x `mod` 2)
	(toList ((fromLists [w]) `multStd` (transpose hm)))

{- makes 9-digit code from 5-digit word -}
code :: [Int] -> [Int]
code w = map (\x -> x `mod` 2) $
	toList ((fromLists [w]) `multStd` gm)

{- encode single 5-bit char -}
encodeChar :: [Int] -> [Int]
encodeChar w = code w

{- correct 9-bit char and get syndrome -}
correctChar :: [Int] -> ([Int], Int)
correctChar w = if (syn <= (length w) && syn >= 0)
	then ((fst s) ++ [binReverse (w !! syn)] ++ (tail . snd $ s), syn)
	else (w,syn)
	where
		syndrome = decode $ w
		syn = if syndrome == [0,0,0,0] then zeroSyn
				else findElem syndrome $ toLists . transpose $ hm
		s = splitAt syn w
		binReverse 1 = 0
		binReverse _ = 1

{- get 5-bit message from 9-bit word -}
decodeChar :: [Int] -> [Int]
decodeChar w = take 5 . fst . correctChar $ w

{- inital call for decFromBin -}
decFromBin :: [Int] -> Int
decFromBin w = _decFromBin w 0

{- recursive getFromBin -}
_decFromBin :: [Int] -> Int -> Int
_decFromBin [] n = n
_decFromBin (b:bs) n = _decFromBin bs $ 2*n + b

binFromDec :: Int -> [Int]
binFromDec n = _binFromDec n []

_binFromDec :: Int -> [Int] -> [Int]
_binFromDec 0 bs = (take (5-len) [0,0..]) ++ bs
	where
		len = length bs
_binFromDec n bs = _binFromDec (n `div` 2) ((n `mod` 2):bs)

binToString :: [Int] -> [Char]
binToString x = map intToDigit x

corrupt :: Int -> [Int] -> [Int]
corrupt i xs = (init . take i $ xs) ++ [rev (xs !! ((length xs) - i))]
		++ (drop i xs)
	where
		rev :: Int -> Int
		rev x = if x == 0 then 1 else 0
