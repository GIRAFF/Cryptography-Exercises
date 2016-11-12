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

gm :: Matrix Int
gm = fromLists
	[[1,0,0,0,0,0,0,1,1]
	,[0,1,0,0,0,0,1,0,1]
	,[0,0,1,0,0,0,1,1,0]
	,[0,0,0,1,0,0,1,1,1]
	,[0,0,0,0,1,1,1,1,0]]

{- gives 4-digits syndrome from 9-digits word -}
decode :: [Int] -> [Int]
{-code x = map (\x -> x `mod` 2) (toList (hm `multStd`-}
		{-(transpose . fromLists $ [x])))-}
decode w = map (\x -> x `mod` 2)
	(toList ((fromLists [w]) `multStd` (transpose hm)))

code :: [Int] -> [Int]
code w = map (\x -> x `mod` 2) $
	toList ((fromLists [w]) `multStd` gm)

{- inital form of _merge -}
{-merge :: [Int] -> [Int] -> [Int]-}
{-merge w c = _merge w c 1-}

{-{- inserts elements of check-word into a data word. Initiate with n = 1. -}-}
{-_merge :: [Int] -> [Int] -> Int -> [Int]-}
{-_merge word [] _ = word-}
{-_merge word (c:cs) n = _merge ((fst s) ++ [c] ++ (snd s)) cs (n*2)-}
	{-where-}
		{-s = splitAt (n-1) word-}

{- encode single 5-bit char -}
encodeChar :: [Int] -> [Int]
{-encodeChar w = merge w . reverse . code $ (merge w [0,0,0,0])-}
encodeChar w = code w

{- correct 9-bit char and get syndrome -}
correctChar :: [Int] -> ([Int], Int)
correctChar w = if (syn /= 0)
	then ((fst s) ++ [binReverse (w !! (syn-1))] ++ (tail . snd $ s), syn)
	else (w,syn)
	where
		syn = (decFromBin . decode $ w)
		s = splitAt (syn-1) w
		binReverse 1 = 0
		binReverse _ = 1

{- strip elements on power-of-2 positions -}
{-stripWord :: [Int] -> [Int]-}
{-stripWord w = fst (unzip (filter (not . isPow . snd) (zip w [1..]))) -}
	{-where-}
		{-isPow x = elem x $ take 4 . map (2^) $ [0..]-}

{- get 5-bit message from 9-bit word -}
decodeChar :: [Int] -> [Int]
decodeChar w = take 5 . fst . correctChar $ w
{-decodeChar w = decode w-}

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
