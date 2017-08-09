module Main where

import System.IO (stdin, stdout)
import Lib (runCLI, parseOptions)

main :: IO ()
main = do
  options <- parseOptions
  runCLI stdin stdout options
