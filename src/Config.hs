{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as L
import Data.Time (getCurrentTime)
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Exit (die)

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

readConfigFile :: IO Configuration
readConfigFile = do
  time <- buildTime
  content <- L.readFile "config.json"
  case eitherDecode content of
    Right conf -> pure conf
    Left err -> do
      let str = time ++ " UTC   " ++ "ERROR  " ++ " - " ++ err
      putStrLn str
      appendFile logFile $ str ++ "\n"
      die "Error reading the configuration file! Check out config.json!"

-- Get current time for the logger.
buildTime :: IO String
buildTime = take 19 . show <$> getCurrentTime

-- Data type for the logger.
data Priority = DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- Name of the logfile.
logFile :: String
logFile = "log.log"
