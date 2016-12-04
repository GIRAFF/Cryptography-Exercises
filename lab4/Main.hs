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
import Code

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	{- init graphics -}
	static <- getStaticDir
	startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
	return w # set UI.title "Шифрование"
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
	im <- UI.input #. "msg" # set (attr "placeholder") "Сообщение (а-я)"
	dm <- UI.input #. "demsg" # set (attr "placeholder") "Сообщение (a-z)"
	bc <- UI.button #. "encode" #+ [string "Закодировать"]
	bd <- UI.button #. "decode" #+ [string "Раскодировать"]
	{- paragraph is needed for correct align -}
	p <- UI.p #+ map element [bc, bd]

	{- input control for alphabet -}
	on (domEvent "input") im $ \_ -> do
		val <- get value im
		element im # set value 
			(controlCommas val)

	on (domEvent "input") dm $ \_ -> do
		val <- get value dm
		element dm # set value 
			(controlCommas val)

	{- encode event -}
	on UI.click bc $ \_ -> do
		msg <- get value im
		element dm # set value
			(intercalate "," .
				encode defAlph defKey . splitOn "," $ msg
			)
		return ()
		
	{- decode event -}
	on UI.click bd $ \_ -> do
		msg <- get value dm
		element im # set value
			(intercalate "," .
				encode defKey defAlph . splitOn "," $ msg
			)
		return ()

	{- return list of created elements -}
	return [im, p, dm]
	where
		{- replace every space with comma -}
		controlCommas val = replace ",," "," $ replace " " "," val
		{- erase comma from the end of string -}
		eraseFinalComma val = if (val /= "" && last val == ',')
			then init val else val
