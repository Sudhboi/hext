module Main (main) where

import App
import Loop (loopApp)

main :: IO ()
main = do
  app <- genApp ""
  loopApp app
  return ()
