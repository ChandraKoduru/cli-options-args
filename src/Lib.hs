module Lib 
  (parseOptions
  , CommandOptions(CommandOptions)
  , runCLI)
where

import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Semigroup ((<>))
import Options.Applicative (execParser, info, helper, Parser, fullDesc, 
  progDesc, short, metavar, flag, argument, str, option)
import System.IO (writeFile, Handle, hPutStrLn, hGetLine)

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

getMessage :: Handle -> Handle -> IO String
getMessage inHandle outHandle = do
  hPutStrLn outHandle "What message do you want in the file?"
  hGetLine inHandle

getRepetitions :: Handle -> Handle -> IO Int
getRepetitions inHandle outHandle = do
  hPutStrLn outHandle "How many times should it be repeated?"
  getNumber inHandle outHandle

getNumber :: Handle -> Handle -> IO Int
getNumber inHandle outHandle = do
  input <- hGetLine inHandle 
  case readMaybe input of
    Nothing -> do
      hPutStrLn outHandle "Sorry, that isn't a valid number. Please enter a number."
      getNumber inHandle outHandle
    Just i -> return i

runCLI :: Handle -> Handle -> CommandOptions -> IO ()
runCLI inHandle outHandle commandOptions = do
  let file = fileName commandOptions
  let user = userName commandOptions
  let uppercase = isUpperCase commandOptions
  message <- getMessage inHandle outHandle
  reps <- getRepetitions inHandle outHandle
  writeFile file (fileContents user message reps uppercase)

fileContents :: String -> String -> Int -> Bool -> String
fileContents user message repetitions uppercase = unlines $ 
  ("From: " ++ user) : (replicate repetitions finalMessage)
  where
    finalMessage = if uppercase 
                   then map toUpper message
                   else message
