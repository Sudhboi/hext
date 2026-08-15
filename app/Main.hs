module Main (main) where

import App
import Loop (initLoop, loopApp)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  app <-
    genApp
      ( case args of
          (x : _) -> x
          _ -> ""
      )
  initLoop app
  loopApp app
  return ()
