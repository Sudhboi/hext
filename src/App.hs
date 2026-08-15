{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Lens (makeLenses, (^.))
import GHC.IO.Handle (hGetContents')
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Graphics.Vty (DisplayRegion, Output (displayBounds), Vty (outputIface), defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (doesFileExist)

data Status = Looping | Done deriving (Show)

data VCursor = VCursor {_line :: Int, _pos :: Int} deriving (Show)

data App = App
  { _status :: Status
  , _editortext :: [String]
  , _cursor :: VCursor
  , _term :: Vty
  , _fileName :: String
  , _bounds :: DisplayRegion
  }

instance Show App where
  -- show :: App -> String
  show app = unlines $ [show . _status, show . _editortext, show . _cursor, show . _fileName, show . _bounds] <*> [app]

$(makeLenses ''VCursor)
$(makeLenses ''App)

genApp :: String -> IO App
genApp file = do
  exists <- doesFileExist file
  contents <- case exists of
    True -> withFile file ReadMode hGetContents'
    False -> return "hi hello \nbruh"
  vty <- mkVty defaultConfig
  bnds <- displayBounds $ outputIface vty
  return (App Looping (lines contents) (VCursor 0 0) vty file bnds)

getText :: App -> [String]
-- getText app = concat $ app ^. editortext
getText = _editortext
