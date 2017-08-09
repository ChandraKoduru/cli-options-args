module Main where

import Lib (runCLI, CommandOptions(CommandOptions))
import System.IO (openFile, IOMode(ReadMode, WriteMode), readFile, hClose)
import Test.HUnit(assertEqual)
import System.FilePath((</>), joinPath)

testRootDir :: FilePath
testRootDir = joinPath ["test", "test1"]

filePathFor :: FilePath -> FilePath
filePathFor s = testRootDir </> s

main :: IO ()
main = do
  inputHandle <- openFile (filePathFor "test_input.txt") ReadMode
  outputHandle <- openFile (filePathFor "actual_terminal.txt") WriteMode
  runCLI inputHandle outputHandle options
  hClose inputHandle
  hClose outputHandle
  expectedTerminal <- readFile $ filePathFor "expected_terminal.txt"
  actualTerminal <- readFile $ filePathFor "actual_terminal.txt"
  expectedFile <- readFile $ filePathFor "expected_output.txt"
  actualFile <- readFile $ filePathFor "actual_output.txt"
  assertEqual "Terminal output should match" expectedTerminal actualTerminal
  assertEqual "Output file should match" expectedFile actualFile


options :: CommandOptions
options = CommandOptions (filePathFor "actual_output.txt") "John Doe" False

