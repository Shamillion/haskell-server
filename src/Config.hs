{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as L
import Data.Time (getCurrentTime)
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Exit (die)

-- Data type for the configuration file.
data Configuration = Configuration
  { serverPort :: Int,
    dbHost :: String,
    dbPort :: Word16,
    dbname :: String,
    dbUser :: String,
    dbPassword :: String,
    maxElem :: Int,
    priorityLevel :: Priority,
    logOutput :: String
  }
  deriving (Show, Generic, FromJSON)

-- Getting information from configuration file.
readConfigFile :: IO Configuration
readConfigFile = do
  time' <- time
  content <- L.readFile "config.json"
  case eitherDecode content of
    Right conf -> pure conf
    Left err -> do
      let str = time' ++ " UTC   " ++ "ERROR  " ++ " - " ++ err
      print str
      appendFile logFile $ str ++ "\n"
      die "Error reading the configuration file! Check out config.json!"

-- Get current time for the logger.
time :: IO String
time = take 19 . show <$> getCurrentTime

-- Maximum number of items returned by the database in response to a request.
limitElem :: IO Int
limitElem = maxElem <$> readConfigFile

-- Data type for the logger.
data Priority = DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- Name of the logfile.
logFile :: String
logFile = "log.log"

-- Function writes log information down.
writingLine :: Priority -> String -> IO ()
writingLine level str = do
  logLevel' <- logLevel
  if level >= logLevel'
    then do
      t <- time
      let string = t <> " UTC   " <> showLevel level <> " - " <> str
      out <- logOutput <$> readConfigFile
      case out of
        "file" -> appendFile logFile $ string <> "\n"
        _ -> putStrLn string
    else pure ()
  where
    showLevel priority = case priority of
      DEBUG -> "DEBUG  "
      INFO -> "INFO   "
      WARNING -> "WARNING"
      ERROR -> "ERROR  "

writingLineDebug :: (Show a) => a -> IO ()
writingLineDebug s = writingLine DEBUG $ show s

-- Logging level.
logLevel :: IO Priority
logLevel = priorityLevel <$> readConfigFile
