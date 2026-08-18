module Events where

import App (App, Status (Done), bounds, currentLine, currentStart, cursor, editortext, line, pos, resetCPos, status, textLine)
import Control.Lens (over, (^.))
import Graphics.Vty (regionHeight)
import Graphics.Vty.Input.Events (
  Event (EvKey, EvResize),
  Key (KBS, KChar, KDown, KEnter, KLeft, KRight, KUp),
  Modifier (MCtrl),
 )
import List (insertAt, mergeAndFlatten, removeAt, replaceAt, splitAndFlatten)

handleEvent :: Event -> App -> App
handleEvent e =
  case e of
    (EvKey (KChar 'c') [MCtrl]) -> quitApp
    (EvResize w h) -> resizeApp w h
    (EvKey KUp []) -> moveCursorUp
    (EvKey KDown []) -> moveCursorDown
    (EvKey KRight []) -> resetCPos . moveCursorRight
    (EvKey KLeft []) -> moveCursorLeft . resetCPos
    (EvKey (KChar ch) []) -> moveCursorRight . insertChar ch . resetCPos
    (EvKey KEnter []) -> over (cursor . pos) (const 0) . moveCursorDown . insertEnter
    (EvKey KBS []) -> backSpace . resetCPos
    _ -> id

quitApp :: App -> App
quitApp = over status (const Done)

resizeApp :: Int -> Int -> App -> App
resizeApp w h = over bounds (const (w, h))

moveCursorUp :: App -> App
moveCursorUp app
  | app ^. cursor . line == 0 = if app ^. currentStart == 0 then app else over currentStart (+ (-1)) app
  | otherwise = over (cursor . line) (+ (-1)) app

moveCursorDown :: App -> App
moveCursorDown app
  | textLine app > length (app ^. editortext) - 2 = app
  | (app ^. cursor . line == (regionHeight (app ^. bounds) - 3))
      && (app ^. currentStart - (regionHeight (app ^. bounds) - 2) <= length (app ^. editortext)) =
      over currentStart (+ 1) app
  | otherwise = over (cursor . line) (+ 1) app

moveCursorRight :: App -> App
moveCursorRight = over (cursor . pos) (+ 1)

moveCursorLeft :: App -> App
moveCursorLeft newApp
  | (newApp ^. cursor . pos) > 0 = over (cursor . pos) (+ (-1)) newApp
  | otherwise = newApp

insertChar :: Char -> App -> App
insertChar ch app = over editortext (const $ replaceAt (textLine app) newStr (app ^. editortext)) app
 where
  newStr = insertAt (app ^. cursor . pos) ch (currentLine app)

insertEnter :: App -> App
insertEnter app = over editortext (const $ splitAndFlatten (textLine app) (app ^. cursor . pos) (app ^. editortext)) app

backSpace :: App -> App
backSpace app
  | (app ^. cursor . pos) == 0 && textLine app == 0 = app
  | (app ^. cursor . pos) == 0 = (moveCursorUp . lineBackSpace) app
  | otherwise = (moveCursorLeft . normalBackSpace) app

normalBackSpace :: App -> App
normalBackSpace app = over editortext (const $ replaceAt (textLine app) newText (app ^. editortext)) app
 where
  newText = removeAt (app ^. cursor . pos) oldText
  oldText = currentLine app

lineBackSpace :: App -> App
lineBackSpace app = (over (cursor . pos) (const oldTextLength) . over editortext (const $ mergeAndFlatten (textLine app) (app ^. editortext))) app
 where
  oldTextLength = length $ (app ^. editortext) !! (textLine app - 1)
