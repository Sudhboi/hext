module Draw where

import App (App, bounds, fileName, getText)
import Control.Lens
import Graphics.Vty (
  Background (ClearBackground),
  Cursor (Cursor),
  Image,
  Picture (Picture),
  blue,
  defAttr,
  regionWidth,
  string,
  withBackColor,
  (<->),
 )

genBar :: String -> App -> Image
genBar str app = string (defAttr `withBackColor` blue) (fill left ++ str ++ fill right)
 where
  fill n = replicate n ' '
  totalWidth = regionWidth (app ^. bounds)
  left = (totalWidth `div` 2) - (length str `div` 2)
  right = totalWidth - (left + length str)

genTopBar :: App -> Image
genTopBar app = genBar toDisplay app
 where
  toDisplay = "hext v0.0.1    " ++ (if app ^. fileName == "" then "UNSAVED" else "(" ++ app ^. fileName ++ ")")

genEditorImage :: App -> Image
genEditorImage = string defAttr . getText

genLayer :: App -> Image
genLayer app = genTopBar app <-> genEditorImage app

genPicture :: App -> Picture
genPicture app = Picture (Cursor 0 0) [genLayer app] ClearBackground
