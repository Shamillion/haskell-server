{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Config where


import Data.Aeson
import qualified Data.ByteString.Lazy      as L
import           Database.PostgreSQL.Simple 
import           System.IO.Unsafe                              (unsafePerformIO)
import           GHC.Generics                                          (Generic)
import           Data.Monoid                                              ((<>))
import           Data.Time                                      (getCurrentTime)
--import qualified Data.Text as T
import           Data.Word                                              (Word16)


data Configuration = Configuration -- Data type for the configuration file.
  { dbHost :: String  
  , dbPort :: Word16
  , dbname :: String 
  , dbUser :: String 
  , dbPassword :: String 
  , maxElem :: Int
--  , priorityLevel :: Priority
--  , logOutput :: T.Text
  }
  deriving (Show, Generic, FromJSON)
 

getConfiguration :: String -> Either String Configuration
getConfiguration fileName =              -- Function reads configuration
  unsafePerformIO $ do                   --  information from file.
    t <- time
    content <- L.readFile fileName
    let obj = eitherDecode content
    case obj of
      Right _ -> pure obj
      Left e  -> do
        let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ e
        print str
  --      hPutStrLn file str
  --      hFlush file
        pure obj

errorConfig :: Configuration   -- The object is used when the configuration
errorConfig =                  --   file is read unsuccessfully.
  Configuration 
    { dbHost = "Error"
    , dbPort = 0
    , dbname = "Error"
    , dbUser = "Error"
    , dbPassword = "Error"
    , maxElem  = 0
 --   , priorityLevel = ERROR
 --   , logOutput = "cons"
    }

configuration :: Configuration              -- Try to read configuration file.
configuration =
  case getConfiguration "../config.json" of
    Right v -> v
    Left e  -> errorConfig

time :: IO String                             -- Get current time for the logger.
time = take 19 . show <$> getCurrentTime


connectInfo :: ConnectInfo
connectInfo =
  ConnectInfo
    { connectHost = dbHost configuration
    , connectPort = dbPort configuration
    , connectDatabase = dbname configuration
    , connectUser = dbUser configuration
    , connectPassword = dbPassword configuration
    }


connectDB :: IO Connection
connectDB = connect connectInfo

limitElem = maxElem configuration
