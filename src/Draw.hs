module Draw where

import App (App, editortext)
import Control.Lens
import Graphics.Vty (Background (ClearBackground), Cursor (Cursor), Image, Picture (Picture), defAttr, string)

genEditorImage :: App -> Image
genEditorImage app = string defAttr (concat $ app ^. editortext)

genPicture :: App -> Picture
genPicture app = Picture (Cursor 0 0) [genEditorImage app] ClearBackground
