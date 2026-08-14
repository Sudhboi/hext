module Events where

import App (App, Status (Done), status)
import Control.Lens (over)
import Graphics.Vty.Input.Events (
  Event (EvKey),
  Key (KChar),
  Modifier (MCtrl),
 )

handleEvent :: Event -> App -> App
handleEvent e =
  case e of
    (EvKey (KChar 'c') [MCtrl]) -> quitApp
    _ -> id

quitApp :: App -> App
quitApp = over status (const Done)
