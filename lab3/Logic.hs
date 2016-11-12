{-# OPTIONS_GHC -Wno-tabs #-}

module Logic where

import Data.List
import Data.Char

{- class for pair of (symbol,probability), comparing by prob -}
data Symbol = Symbol (String, Double) deriving (Show)
instance Eq Symbol where
	(Symbol (_, d1)) == (Symbol (_, d2)) = d1 == d2
instance Ord Symbol where
	(Symbol (_, d1)) `compare` (Symbol (_, d2)) = d1 `compare` d2

{- class for Tree -}
data Tree =
	Nodes [Symbol]
	| Node Symbol
	| NodeInner (Tree, Tree)
	deriving (Show)

{- getter for probability -}
symWeight :: Symbol -> Double
symWeight (Symbol (_, w)) = w

{- sum of probabilities -}
weight :: [Symbol] -> Double
weight l = sum (map symWeight l)

{- make node with close weights of branches -}
balance :: [Symbol] -> [Symbol] -> Tree
balance l r
	| length l == 1 && length r == 1 =
		NodeInner (Node . head $ l, Node . head $ r)
	| weight l >= weight r =
		if length l == 1
		then NodeInner (Node . head $ l, Nodes r)
		else if length r == 1
		then NodeInner (Nodes l, Node . head $ r)
		else NodeInner (Nodes l, Nodes r)
	| otherwise = balance (l ++ [head r]) (tail r)

{- making tree from your Nodes [Symbol] -}
{- don't forget to sort -}
buildTree :: Tree -> Tree
buildTree (Node x) = Node x
buildTree (Nodes xs) = buildTree (balance [head xs] (tail xs))
buildTree (NodeInner (x, y)) = NodeInner ((buildTree x), (buildTree y))

{- correct way to make tree out of your [Symbol] -}
buildTreeFromList :: [Symbol] -> Tree
buildTreeFromList l = buildTree . Nodes . reverse . sort $ l

{- build table of codes for tree -}
{- codeList tree [] for inital -}
codeList :: Tree -> [Int] -> [(String, [Int])]
codeList (NodeInner (l,r)) code = codeList l (code ++ [1]) ++ codeList r (code ++ [0])
codeList (Node (Symbol (s, _))) code = [(s,code)] 

{- get code of symbol from codeList -}
getCode :: [(String, [Int])] -> String -> [Int]
getCode alph c
	| null matches = []
	| otherwise =  snd . head $ matches
	where
		matches = (filter (\(s, _) -> s == c) alph)

{- encode message with given codeList -}
encode :: [(String, [Int])] -> [String] -> [Int]
encode alph msg = foldl (++) [] (
		map (\x -> x ++ [(foldl (+) 0 x) `mod` 2])
		(map (getCode alph) msg)
		)

{- decode message with given tree -}
decode :: Tree -> [Int] -> [String]
decode t [] = []
decode t bs = fst dc : decode t (snd dc)
	where
		dc = decodeChar t bs 0

{- get char from given code -}
decodeChar :: Tree -> [Int] -> Int -> (String, [Int])
decodeChar (NodeInner(l, r)) (b:bs) par =
	if b == 1
	then decodeChar l bs (par+1)
	else decodeChar r bs par
decodeChar (Node(Symbol(c, _))) (b:bs) par =
	if b == par `mod` 2
	then (c, bs)
	else ("NUL",bs)

{-validMsg :: [String] -> [String] -> Bool-}
{-validMsg alph msg-}
	{-| all (\c -> c `elem` alph) msg = True-}
	{-| otherwise = False-}
