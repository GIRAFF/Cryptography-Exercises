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

{- size of canvas -}
canvasSize = (800,600)
{- coords of error message on canvas -}
errorPoint = (20,20)
{- coords of tree's root on canvas -}
rootPoint = (400,10)

main :: IO ()
main = do
	{- init graphics -}
	static <- getStaticDir
	startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
	return w # set UI.title "Кодирование"
	UI.addStyleSheet w "code.css"
	{- make canvas -}
	canvas <- UI.canvas #. "tree" #
		set UI.width (fst canvasSize) #
		set UI.height (snd canvasSize)
	{- make control elements -}
	ctrls <- mkCtrls canvas
	{- add created elements to the document -}
	getBody w #+
		[UI.div #. "div-input" #+ map element ctrls,
		 UI.div #. "div-tree"  #+ [element canvas]]

{- the only argument is a canvas for drawing -}
mkCtrls :: Element -> UI [Element]
mkCtrls c = do
	{- creating all the buttons and input fields -}
	ia <- UI.input #. "alph" # set (attr "placeholder") "Алфавит"
	ip <- UI.input #. "prob" # set (attr "placeholder") "Вероятности"
	im <- UI.input #. "msg" # set (attr "placeholder") "Сообщение"
	ic <- UI.input #. "code" # set (attr "placeholder") "Код"
	bc <- UI.button #. "code" #+ [string "Закодировать"]
	bd <- UI.button #. "decode" #+ [string "Раскодировать"]
	{- paragraph is needed for correct align -}
	p <- UI.p #+ map element [bc, bd]

	{- input control for alphabet -}
	on (domEvent "input") ia $ \_ -> do
		ia_val <- get value ia
		element ia # set value (controlCommas ia_val)

	{- input control for probabilities -}
	on (domEvent "input") ip $ \_ -> do
		ip_val <- get value ip
		element ip # set value
			(filter (\x -> isDigit x || x == '.' || x == ',')
			(replace ".," "," (controlCommas ip_val)))
		if ip_val == "." then do
			element ip # set value "0."
			return ()
		else
			when (ip_val /= ""
					&& (last ip_val == '.')
					&& not (isDigit (last . init $ ip_val))) $
				do
					element ip # set value (init ip_val ++ "0.")
					return ()

	{- input control for message -}
	on (domEvent "input") im $ \_ -> do
		im_val <- get value im
		element im # set value (controlCommas im_val)

	{- input control for encrypted message -}
	{-on (domEvent "input") ic $ \_ -> do
		ic_val <- get value ic
		element ic # set value (filter isDigit (controlCommas ic_val))-}
	{- final comma erasers for every field -}
	on (domEvent "focusout") ia $ \_ -> do
		val <- get value ia
		element ia # set value (eraseFinalComma val)

	on (domEvent "focusout") ip $ \_ -> do
		val <- get value ip
		element ip # set value (eraseFinalComma val)

	on (domEvent "focusout") im $ \_ -> do
		val <- get value im
		element im # set value (eraseFinalComma val)

	on (domEvent "focusout") ic $ \_ -> do
		val <- get value ic
		element ic # set value (eraseFinalComma val)

	{- encode event -}
	on UI.click bc $ \_ -> do
		im_val <- get value im
		ia_val <- get value ia
		ip_val <- get value ip
		{- split String to [String] -}
		let msg = splitOn "," (filter (\x -> x /= ' ') im_val)
		{- get alphabet [Symbol] -}
		let alph = getAlphFromStr ia_val ip_val
		c # UI.clearCanvas
		{- if something is wrong with alphabet -}
		if alph == Nothing
		then showErrorMsg
		else do
			let tree = buildTreeFromList (head . maybeToList $ alph)
			{- root came from nowhere, so we can use both True and False -}
			drawTree c False tree rootPoint
			let codes = codeList tree []
			{- encode -}
			element ic # set value
				(intercalate "," (map show (encode codes msg)))
			return ()

	{- decode event -}
	on UI.click bd $ \_ -> do
		ic_val <- get value ic
		ia_val <- get value ia
		ip_val <- get value ip
		let code = map (\x -> read x :: Int)
			(splitOn "," (filter (\x -> x /= ' ') ic_val))
		let alph = getAlphFromStr ia_val ip_val
		c # UI.clearCanvas
		if alph == Nothing
		then showErrorMsg
		else do
			let tree = buildTreeFromList (head . maybeToList $ alph)
			drawTree c False tree rootPoint 
			element im # set value (intercalate "," (decode tree code))
			return ()

	{- return list of created elements -}
	return [ia, ip, im, p, ic]
	where
		{- replace every space with comma -}
		controlCommas val = replace ",," "," $ replace " " "," val
		{- erase comma from the end of string -}
		eraseFinalComma val = if (val /= "" && last val == ',')
			then init val else val
		{- show error message -}
		showErrorMsg = do
			c # set' UI.fillStyle (UI.htmlColor "red")
			c # set' UI.textFont "12px sans-serif"
			c # UI.fillText "Ошибка: сумма вероятностей > 1." errorPoint

{- try to make [Symbol] from "a1,a2..aN" and "p(a1),p(a2)..p(aN)" -}
getAlphFromStr :: String -> String -> Maybe [Symbol]
getAlphFromStr alph prob 
	{- sum of probabilities cannot be more than one -}
	| sum ps > 1 = Nothing
	| otherwise = Just (rmDup . filter (\(Symbol (_,w)) -> w /= 0) $
		(map (Symbol) (zip as ps)))
	where
		as = splitOn "," (filter (\x -> x /= ' ') alph)
		ps = map (\x -> read x :: Double) (splitOn "," prob)
		{- remove duplicate symbols from list -}
		rmDup :: [Symbol] -> [Symbol]
		rmDup [] = []
		rmDup ((Symbol (x,w)):xs) =
			(Symbol (x,w)) : rmDup (filter (\(Symbol (s,_)) -> s /= x) xs)

{- "radiius" of a node -}
dr = 5
dx = 30
dy = 60

{- TODO make nice tree -}
drawTree :: Element -> Bool -> Tree -> (Double, Double) -> UI ()
drawTree c _ (NodeInner (l, r)) (x, y) = do
	drawBranches c (x, y)
	{-drawNode c (x, y)-}
	drawTree c False l (x-dx, y+dy)
	drawTree c True r (x+dx, y+dy)
drawTree c side (Node (Symbol (s,_))) (x, y) = do
	drawNode c (x, y)
	c # set' UI.fillStyle (UI.htmlColor "#8787af")
	c # set' UI.textFont "12px sans-serif"
	c # UI.fillText s (x+(if side then 2*dr else -3*dr),y)

drawNode :: Element -> (Double, Double) -> UI ()
drawNode c (x, y) = do
	c # set' UI.fillStyle (UI.htmlColor "#77779f")
	c # UI.fillRect (x-dr,y-dr) (2*dr) (2*dr)

drawBranches :: Element -> (Double, Double) -> UI ()
drawBranches c (x, y) = do
	c # set' UI.strokeStyle "#ffaf5f"
	c # UI.beginPath
	c # UI.moveTo (x,y)
	c # UI.lineTo (x-dx,y+dy)
	c # UI.closePath
	c # UI.stroke
	c # set' UI.strokeStyle "#ffd787"
	c # UI.beginPath
	c # UI.moveTo (x,y)
	c # UI.lineTo (x+dx,y+dy)
	c # UI.closePath
	c # UI.stroke
