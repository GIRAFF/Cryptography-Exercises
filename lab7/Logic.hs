{-# OPTIONS_GHC -Wno-tabs #-}

import Prime

fermat :: Int -> [Int]
fermat n =
	_fermat (2*(floor . sqrt . fromIntegral $ n) + 1) 1
		((floor . sqrt . fromIntegral $ n)^2 - n)

_fermat :: Int -> Int -> Int -> [Int]
_fermat x y r
	| r == 0 = [(x - y) `div` 2, (x + y - 2) `div` 2]
	| r > 0 = _fermat x (y+2) (r-y)
	| otherwise = _fermat (x+2) (y+2) (r + x - y)
