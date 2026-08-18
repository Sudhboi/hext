module Draw where

import App (App, bounds, currentLine, currentStart, cursor, fileName, getText, line, pos)
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
genBotBar app = string (defAttr `withBackColor` blue) (fill 2 ++ info ++ fill rest ++ cursorPos ++ fill 2)
 where
  info = "^C Quit        ^S Save"
  cursorPos = "(" ++ show (app ^. cursor . pos) ++ "," ++ show ((app ^. cursor . line) + (app ^. currentStart)) ++ ")"
  width = regionWidth $ app ^. bounds
  rest = width - (length info + length cursorPos + 4)

genEditorImage :: App -> Image
genEditorImage app = vertCat $ map (string defAttr) (getText app)

genLayer :: App -> Image
genLayer app = vertCat $ [genTopBar, genEditorImage, genBotBar] <*> [app]

genPicture :: App -> Picture
genPicture app = Picture (renderCursor app) [genLayer app] ClearBackground

renderCursor :: App -> Cursor
renderCursor app = Cursor (min p (length $ currentLine app)) (1 + app ^. cursor . line)
 where
  p = app ^. cursor . pos
