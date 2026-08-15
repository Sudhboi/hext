module Events where

import App (App, Status (Done), bounds, currentStart, cursor, editortext, line, status)
import Control.Lens (over, (^.))
import Graphics.Vty (regionHeight)
import Graphics.Vty.Input.Events (
  Event (EvKey, EvResize),
  Key (KChar, KDown, KUp),
  Modifier (MCtrl),
 )

handleEvent :: Event -> App -> App
handleEvent e =
  case e of
    (EvKey (KChar 'c') [MCtrl]) -> quitApp
    (EvResize w h) -> resizeApp w h
    (EvKey KUp []) -> moveCursorUp
    (EvKey KDown []) -> moveCursorDown
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
  | app ^. cursor . line == (regionHeight (app ^. bounds) - 3) =
      if app ^. currentStart == length (app ^. editortext) - 1 then app else over currentStart (+ 1) app
  | otherwise = over (cursor . line) (+ 1) app
