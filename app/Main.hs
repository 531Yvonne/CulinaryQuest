module Main where

import Control.Monad.State
import GameIO
import GameState
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  evalStateT opening initialState
  evalStateT (forever repl) initialState
