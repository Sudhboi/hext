{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Lens (makeLenses)
import Graphics.Vty (Vty, defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)

data Status = Looping | Done

data VCursor = VCursor {_line :: Int, _pos :: Int}

data App = App
  { _status :: Status
  , _editortext :: [String]
  , _cursor :: VCursor
  , _term :: Vty
  , _fileName :: String
  }

$(makeLenses ''VCursor)
$(makeLenses ''App)

genApp :: String -> IO App
genApp file = do
  vty <- mkVty defaultConfig
  return (App Looping [] (VCursor 0 0) vty file)
