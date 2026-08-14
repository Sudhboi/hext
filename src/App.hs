{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Lens (makeLenses, (^.))
import Graphics.Vty (DisplayRegion, Output (displayBounds), Vty (outputIface), defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)

data Status = Looping | Done

data VCursor = VCursor {_line :: Int, _pos :: Int}

data App = App
  { _status :: Status
  , _editortext :: [String]
  , _cursor :: VCursor
  , _term :: Vty
  , _fileName :: String
  , _bounds :: DisplayRegion
  }

$(makeLenses ''VCursor)
$(makeLenses ''App)

genApp :: String -> IO App
genApp file = do
  vty <- mkVty defaultConfig
  bnds <- displayBounds $ outputIface vty
  return (App Looping [] (VCursor 0 0) vty file bnds)

getText :: App -> String
-- getText app = concat $ app ^. editortext
getText app = show (app ^. bounds)
