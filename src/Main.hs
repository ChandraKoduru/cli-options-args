module Main where

import Text.Read (readMaybe)
import Data.Char (toUpper)
import System.IO (writeFile)
import Data.Semigroup ((<>))
import Options.Applicative (execParser, info, helper, Parser, fullDesc, 
  progDesc, short, metavar, flag, argument, str, option)

data CommandOptions = CommandOptions
  { fileName :: FilePath
  , userName :: String
  , isUpperCase :: Bool }

upperCaseParser :: Parser Bool
upperCaseParser = flag False True (short 'u')

fileNameParser :: Parser FilePath
fileNameParser = argument str (metavar "FILENAME")

userNameParser :: Parser String
userNameParser = option str (short 'n' <> metavar "USERNAME")

parseOptions :: IO CommandOptions
parseOptions = execParser $ info (helper <*> commandOptsParser) commandOptsInfo
  where
    commandOptsParser = CommandOptions <$> fileNameParser <*> userNameParser <*> upperCaseParser
    commandOptsInfo = fullDesc <> progDesc "Command line sample program"

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

runCLI :: CommandOptions -> IO ()
runCLI commandOptions = do
  let file = fileName commandOptions
  let user = userName commandOptions
  let uppercase = isUpperCase commandOptions
  message <- getMessage 
  reps <- getRepetitions 
  writeFile file (fileContents user message reps uppercase)

fileContents :: String -> String -> Int -> Bool -> String
fileContents user message repetitions uppercase = unlines $ 
  ("From: " ++ user) : (replicate repetitions finalMessage)
  where
    finalMessage = if uppercase 
                   then map toUpper message
                   else message

main :: IO ()
main = do
  options <- parseOptions
  runCLI options

