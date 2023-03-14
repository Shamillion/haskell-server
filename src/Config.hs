{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as L
import Data.Time (getCurrentTime)
import Data.Word (Word16)
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics (Generic)
import qualified System.IO as I


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

-- Function reads configuration information from file.
getConfiguration :: String -> IO (Either String Configuration)
getConfiguration fileName = do
    t <- time
    content <- L.readFile fileName
    let obj = eitherDecode content
    case obj of
      Right _ -> pure obj
      Left e -> do
        let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ e
        print str
        file' <- file
        I.hPutStrLn file' str
        I.hFlush file'
        pure obj

-- The object is used when the configuration
--   file is read unsuccessfully.
errorConfig :: Configuration
errorConfig =
  Configuration
    { serverPort = 0,
      dbHost = "Error",
      dbPort = 0,
      dbname = "Error",
      dbUser = "Error",
      dbPassword = "Error",
      maxElem = 0,
      priorityLevel = ERROR,
      logOutput = "cons"
    }

-- Try to read configuration file.
configuration :: IO Configuration
configuration = do
  getConf <- getConfiguration "config.json"
  pure $ case getConf of
    Right v -> v
    Left _ -> errorConfig
 
-- Get current time for the logger.
time :: IO String
time = take 19 . show <$> getCurrentTime

-- Parameters for connecting to the database.
connectInfo :: IO PS.ConnectInfo
connectInfo = do
  conf <- configuration 
  pure $ PS.ConnectInfo
    { PS.connectHost = dbHost conf,
      PS.connectPort = dbPort conf,
      PS.connectDatabase = dbname conf,
      PS.connectUser = dbUser conf,
      PS.connectPassword = dbPassword conf
    }

-- Maximum number of items returned by the database in response to a request.
limitElem :: IO Int
limitElem = maxElem <$> configuration 

-- Establishing a connection to the database.
connectDB :: IO PS.Connection
connectDB = do
  writingLine INFO "Sent a request to the database."
  connectInfo >>= writingLineDebug 
  connectInfo >>= PS.connect 

-- Data type for the logger.
data Priority = DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- Get Handle for the logfile.
file :: IO I.Handle
{-# NOINLINE file #-}
file = I.openFile "../log.log" I.AppendMode

-- Function writes log information down.
writingLine :: Priority -> String -> IO ()
writingLine lvl str = do
  logLevel' <- logLevel
  if lvl >= logLevel'
    then do
      t <- time
      let string = t ++ " UTC   " ++ fun lvl ++ " - " ++ str
      out <- logOutput <$> configuration
      case out of
        "file" -> do
          file' <- file
          I.hPutStrLn file' string
          I.hFlush file'
        _ -> putStrLn string
    else pure ()
  where    
    fun val = case val of
      DEBUG -> "DEBUG  "
      INFO -> "INFO   "
      WARNING -> "WARNING"
      ERROR -> "ERROR  "

writingLineDebug :: (Show a) => a -> IO ()
writingLineDebug s = writingLine DEBUG $ show s

-- Logging level.
logLevel :: IO Priority
logLevel = priorityLevel <$> configuration 

-- TCP port number.
port :: IO Int
port = serverPort <$> configuration  
