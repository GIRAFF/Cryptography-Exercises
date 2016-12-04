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
import Logic 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	{- init graphics -}
	static <- getStaticDir
	startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
	return w # set UI.title "Псеводслучайные последовательности"
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
	ia <- UI.input #. "msg" # set (attr "placeholder") "a"
	ic <- UI.input #. "msg" # set (attr "placeholder") "c"
	ix <- UI.input #. "msg" # set (attr "placeholder") "x0"
	dm <- UI.p
	bc <- UI.button #. "encode" #+ [string "Сгенерировать"]
	per <- UI.p
	st <- UI.p
	{- paragraph is needed for correct align -}
	p <- UI.p #+ map element [bc]

	{- input control for alphabet -}
	on (domEvent "input") ia $ \_ -> do
		val <- get value ia
		element ia # set value 
			(controlCommas val)

	on (domEvent "input") ic $ \_ -> do
		val <- get value ic
		element ic # set value 
			(controlCommas val)

	on (domEvent "input") ix $ \_ -> do
		val <- get value ix
		element ix # set value 
			(controlCommas val)

	{- encode event -}
	on UI.click bc $ \_ -> do
		ta <- get value ia
		tc <- get value ic
		tx <- get value ix
		let seq = gen (read ta :: Integer) (read tc :: Integer)
					(floor n) [(read tx :: Integer)]
		let pear = pearson seq
		element dm # set text (show seq)
			{-(intercalate "," .-}
				{-encode defAlph defKey . splitOn "," $ msg-}
			{-)-}
		element per # set text ("Период: " ++ (show . period $ seq))
		element st # set text (if (snd pear)
				then ("Good sequence: " ++ (show (fst pear)) ++ " < "
						++ (show scr))
				else "Bad sequence: " ++ (show (fst pear)) ++ " > "
						++ (show scr))
		return ()
		
	{- return list of created elements -}
	return [ia, ic, ix, p, dm, per, st]
	where
		{- replace every space with comma -}
		controlCommas val = replace ",," "," $ replace " " "," val
		{- erase comma from the end of string -}
		eraseFinalComma val = if (val /= "" && last val == ',')
			then init val else val
