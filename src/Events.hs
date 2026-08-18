module Events where

import App (App, Status (Done), bounds, currentStart, cursor, editortext, line, pos, resetCPos, status, textLine)
import Control.Lens (over, (^.))
import Graphics.Vty (regionHeight)
import Graphics.Vty.Input.Events (
  Event (EvKey, EvResize),
  Key (KChar, KDown, KLeft, KRight, KUp),
  Modifier (MCtrl),
 )

handleEvent :: Event -> App -> App
handleEvent e =
  case e of
    (EvKey (KChar 'c') [MCtrl]) -> quitApp
    (EvResize w h) -> resizeApp w h
    (EvKey KUp []) -> moveCursorUp
    (EvKey KDown []) -> moveCursorDown
    (EvKey KRight []) -> moveCursorRight
    (EvKey KLeft []) -> moveCursorLeft
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

-- \| app ^. cursor . line == (regionHeight (app ^. bounds) - 3) =
--     if textLine app == length (app ^. editortext) - 1 then app else over currentStart (+ 1) app
-- \| otherwise = over (cursor . line) (+ 1) app

moveCursorRight :: App -> App
moveCursorRight app = resetCPos $ over (cursor . pos) (+ 1) app

moveCursorLeft :: App -> App
moveCursorLeft app
  | (newApp ^. cursor . pos) > 0 = over (cursor . pos) (+ (-1)) newApp
  | otherwise = newApp
 where
  newApp = resetCPos app
