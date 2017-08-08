module Main where

import Text.Read (readMaybe)
import Data.Char (toUpper)
import System.IO (writeFile)

getMessage :: IO String
getMessage = do
  putStrLn "What message do you want in the file?"
  getLine

getRepetitions :: IO Int
getRepetitions = do
  putStrLn "How many times should it be repeated?"
  getNumber

getNumber :: IO Int
getNumber = do
  input <- getLine
  case readMaybe input of
    Nothing -> do
      putStrLn "Sorry, that isn't a valid number. Please enter a number."
      getNumber 
    Just i -> return i

runCLI :: IO ()
runCLI = do
  let fileName = "myfile.txt"
  let userName = "John Doe"
  let isUpperCase = True
  message <- getMessage 
  reps <- getRepetitions 
  writeFile fileName (fileContents userName message reps isUpperCase)

fileContents :: String -> String -> Int -> Bool -> String
fileContents userName message repetitions isUpperCase = unlines $ 
  ("From: " ++ userName) : (replicate repetitions finalMessage)
  where
    finalMessage = if isUpperCase 
                   then map toUpper message
                   else message

main :: IO ()
main = runCLI

