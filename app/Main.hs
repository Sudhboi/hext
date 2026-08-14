module Main (main) where

import App
import Loop (initLoop, loopApp)

main :: IO ()
main = do
  app <- genApp ""
  initLoop app
  loopApp app
  return ()
