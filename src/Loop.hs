module Loop where

import App (App, Status (Done, Looping), status, term)
import Control.Lens
import Draw (genPicture)
import Events
import Graphics.Vty (Vty (nextEvent, shutdown), update)

loopApp :: App -> IO ()
loopApp app = do
  e <- nextEvent (app ^. term)
  let newApp = handleEvent e app
  update (app ^. term) (genPicture newApp)
  case newApp ^. status of
    Looping -> loopApp newApp
    Done -> shutdown (newApp ^. term)
