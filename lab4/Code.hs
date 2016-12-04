{-# OPTIONS_GHC -Wno-tabs #-}

module Code where

{- finds number of element in list -}
findElem :: Eq a => a -> [a] -> Int
findElem a as = if a `elem` as
		then fst . head . filter (\(_,x) -> x == a) . zip [0..] $ as
		else (-1)

defAlph :: [String]
defAlph = ["_","а","б","в","г","д","е","ё","ж","з","и","й","к","л","м","н",
		"о","п","р","с","т","у","ф","х","ц","ч","ш","щ","ъ","ы","ь",
		"э","ю","я"]

defKey :: [String]
defKey = ["_","a","b","v","g","d","e","yo","zh","z","i","i'","k",
			"l","m","n","o","p","r","s","t","u","f","h","ts","ch","sh",
			"sh'","*","ii","'","ey","yu","ya"]

encode :: [String] -> [String] -> [String] -> [String]
encode alph key msg = map (\x -> snd . head .filter (\(y,_) -> y == x)
					$ zipped) (filter (\x -> x `elem` alph) msg)
	where
		zipped = zip alph key
