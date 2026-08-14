module Events where

import App (App, Status (Done), bounds, status)
import Control.Lens (over)
import Graphics.Vty.Input.Events (
  Event (EvKey, EvResize),
  Key (KChar),
  Modifier (MCtrl),
 )

handleEvent :: Event -> App -> App
handleEvent e =
  case e of
    (EvKey (KChar 'c') [MCtrl]) -> quitApp
    (EvResize w h) -> resizeApp w h
    _ -> id

quitApp :: App -> App
quitApp = over status (const Done)

resizeApp :: Int -> Int -> App -> App
resizeApp w h = over bounds (const (w, h))
