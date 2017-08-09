module Main where

import Lib (runCLI, CommandOptions(CommandOptions))
import System.IO (openFile, IOMode(ReadMode, WriteMode), readFile, hClose)
import Test.HUnit(assertEqual)

main :: IO ()
main = do
  inputHandle <- openFile "test_input.txt" ReadMode
  outputHandle <- openFile "actual_terminal.txt" WriteMode
  runCLI inputHandle outputHandle options
  hClose inputHandle
  hClose outputHandle
  expectedTerminal <- readFile "expected_terminal.txt"
  actualTerminal <- readFile "actual_terminal.txt"
  expectedFile <- readFile "expected_output.txt"
  actualFile <- readFile "actual_output.txt"
  assertEqual "Terminal output should match" expectedTerminal actualTerminal
  assertEqual "Output file should match" expectedFile actualFile


options :: CommandOptions
options = CommandOptions "actual_output.txt" "John Doe" False

