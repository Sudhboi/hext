module Draw where

import App (App, bounds, currentStart, cursor, fileName, getText, line, pos)
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
  vertCat,
  withBackColor,
 )

fill :: Int -> [Char]
fill n = replicate n ' '

genBar :: String -> App -> Image
genBar str app = string (defAttr `withBackColor` blue) (fill left ++ str ++ fill right)
 where
  totalWidth = regionWidth (app ^. bounds)
  left = (totalWidth `div` 2) - (length str `div` 2)
  right = totalWidth - (left + length str)

genTopBar :: App -> Image
genTopBar app = genBar toDisplay app
 where
  toDisplay = "hext v0.0.1    " ++ (if app ^. fileName == "" then "UNSAVED" else "(" ++ app ^. fileName ++ ")")

genBotBar :: App -> Image
genBotBar app = genBar ("^C Quit        ^S Save" ++ fill (regionWidth (app ^. bounds) `div` 3) ++ cursorPos) app
 where
  cursorPos = "(" ++ show (app ^. cursor . pos) ++ "," ++ show ((app ^. cursor . line) + (app ^. currentStart)) ++ ")"

genEditorImage :: App -> Image
genEditorImage app = vertCat $ map (string defAttr) (getText app)

genLayer :: App -> Image
genLayer app = vertCat $ [genTopBar, genEditorImage, genBotBar] <*> [app]

genPicture :: App -> Picture
genPicture app = Picture (Cursor 0 (1 + app ^. cursor . line)) [genLayer app] ClearBackground
