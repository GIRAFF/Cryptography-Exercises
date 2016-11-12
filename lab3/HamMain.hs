{-# OPTIONS_GHC -Wno-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Char
import Data.Maybe

import Paths
import Hamming

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	{- init graphics -}
	static <- getStaticDir
	startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
	return w # set UI.title "Код Хэмминга"
	UI.addStyleSheet w "code.css"
	{- make control elements -}
	ctrls <- mkCtrls
	{- add created elements to the document -}
	getBody w #+
		[UI.div #. "div-input" #+ map element ctrls]

{- the only argument is a canvas for drawing -}
mkCtrls :: UI [Element]
mkCtrls = do
	{- creating all the buttons and input fields -}
	im <- UI.input #. "msg" # set (attr "placeholder") "Сообщение (числа 0-31)"
	ic <- UI.input #. "code" # set (attr "placeholder") "Код Хэмминга"
	dc <- UI.input #. "decode" # set (attr "placeholder") "Исправленный код"
	dm <- UI.input #. "demsg" # set (attr "placeholder") "Полученное сообщение"
	bc <- UI.button #. "encode" #+ [string "Закодировать"]
	bd <- UI.button #. "decode" #+ [string "Раскодировать"]
	{- paragraph is needed for correct align -}
	p <- UI.p #+ map element [bc, bd]

	{- input control for alphabet -}
	on (domEvent "input") im $ \_ -> do
		val <- get value im
		element im # set value 
			(filter (\x -> isDigit x || x == ',') $ controlCommas val)

	{-on (domEvent "input") ic $ \_ -> do
		val <- get value ic
		element ic # set value 
			(filter (\x -> elem x ['0','1',',']) $ controlCommas val)-}

	{- encode event -}
	on UI.click bc $ \_ -> do
		msg <- get value im
		element ic # set value
			(intercalate "," $
				map (binToString . encodeChar . binFromDec)
					(filter (\x -> x < 32 && x >= 0) $
						readListFromString msg)
			)
		return ()
		
	{- decode event -}
	on UI.click bd $ \_ -> do
		str <- get value ic
		let ws =
			map (correctChar . map (\x -> if x == '1' then 1 else 0)) $
				splitOn "," str
		let sp = \x ->
			splitPlaces [(snd x) - 1, 1, length . fst $ x] (fst x)
		element dc # set value
			(intercalate "," (map (\x ->
				if (snd x) == 0
				then binToString . fst $ x
				else foldl (++) "" $
					[binToString ((sp x) !! 0),
					if ((sp x) !! 1) == [0] then "[0]" else "[1]",
					binToString ((sp x) !! 2)]) ws)
			)
		element dm # set value
			(intercalate ","
			 . map (show . decFromBin . stripWord)
			 . fst . unzip $ ws)
		return ()
		{-ic_val <- get value ic-}
		{-ia_val <- get value ia-}
		{-ip_val <- get value ip-}
		{-let code = map (\x -> read x :: Int)-}
			{-(splitOn "," (filter (\x -> x /= ' ') ic_val))-}
		{-let alph = getAlphFromStr ia_val ip_val-}
		{-c # UI.clearCanvas-}
		{-if alph == Nothing-}
		{-then showErrorMsg-}
		{-else do-}
			{-let tree = buildTreeFromList (head . maybeToList $ alph)-}
			{-drawTree c False tree rootPoint -}
			{-element im # set value (intercalate "," (decode tree code))-}
			{-return ()-}

	{- return list of created elements -}
	return [im, ic, p, dc, dm]
	where
		{- replace every space with comma -}
		controlCommas val = replace ",," "," $ replace " " "," val
		readListFromString str = read ("[" ++ str ++ "]") :: [Int]
		{- erase comma from the end of string -}
		eraseFinalComma val = if (val /= "" && last val == ',')
			then init val else val
