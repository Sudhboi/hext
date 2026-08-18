{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Lens (makeLenses, over, (^.))
import GHC.IO.Handle (hGetContents')
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Graphics.Vty (DisplayRegion, Output (displayBounds), Vty (outputIface), defaultConfig, regionHeight)
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (doesFileExist)

data Status = Looping | Done deriving (Show)

data VCursor = VCursor {_pos :: Int, _line :: Int, _cHPos :: Int} deriving (Show)

data App = App
  { _status :: Status
  , _editortext :: [String]
  , _cursor :: VCursor
  , _term :: Vty
  , _fileName :: String
  , _bounds :: DisplayRegion
  , _currentStart :: Int
  }

instance Show App where
  show app =
    unlines $
      ( [ show . _status
        , show . _editortext
        , show . _cursor
        , show . _fileName
        , show . _bounds
        , show . _currentStart
        ]
          <*> [app]
      )
        ++ [show $ length (_editortext app)]

$(makeLenses ''VCursor)
$(makeLenses ''App)

genApp :: String -> IO App
genApp file = do
  exists <- doesFileExist file
  contents <- case exists of
    True -> withFile file ReadMode hGetContents'
    False -> return "\n"
  vty <- mkVty defaultConfig
  bnds <- displayBounds $ outputIface vty
  return
    ( App
        Looping
        (lines contents)
        (VCursor 0 0 0)
        vty
        file
        bnds
        0
    )

getText :: App -> [String]
getText app = take (regionHeight (app ^. bounds) - 2) (drop (app ^. currentStart) (app ^. editortext ++ repeat ""))

textLine :: App -> Int
textLine app = (app ^. cursor . line) + (app ^. currentStart)

currentLine :: App -> String
currentLine app = (app ^. editortext) !! textLine app

resetCPos :: App -> App
resetCPos app = if (app ^. cursor . pos) <= length (currentLine app) then app else over (cursor . pos) (const $ length $ currentLine app) app
