module Loop where

import App (App, Status (Done, Looping, Saving), saveFile, status, term)
import Control.Lens
import Draw (genPicture)
import Events
import Graphics.Vty (Vty (nextEvent, shutdown), update)

initLoop :: App -> IO ()
initLoop app = do
  update (app ^. term) (genPicture app)
  return ()

loopApp :: App -> IO ()
loopApp app = do
  e <- nextEvent (app ^. term)
  let newApp = handleEvent e app
  update (app ^. term) (genPicture newApp)
  case newApp ^. status of
    Looping -> loopApp newApp
    Saving -> do
      newnewApp <- saveFile newApp
      loopApp newnewApp
    Done -> do
      shutdown (newApp ^. term)
      print newApp
