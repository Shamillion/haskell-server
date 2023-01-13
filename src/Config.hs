{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Config where


import           Data.Aeson                 (eitherDecode, FromJSON)
import qualified Data.ByteString.Lazy       as L
import qualified Database.PostgreSQL.Simple as PS
import           Data.Time                  (getCurrentTime)
import           Data.Word                  (Word16)
import           GHC.Generics               (Generic)
import qualified System.IO                  as I
import           System.IO.Unsafe           (unsafePerformIO)




data Configuration = Configuration -- Data type for the configuration file.
  { serverPort :: Int
  , dbHost :: String  
  , dbPort :: Word16
  , dbname :: String 
  , dbUser :: String 
  , dbPassword :: String 
  , maxElem :: Int
  , priorityLevel :: Priority
  , logOutput :: String
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
        I.hPutStrLn file str
        I.hFlush file
        pure obj

errorConfig :: Configuration   -- The object is used when the configuration
errorConfig =                  --   file is read unsuccessfully.
  Configuration 
    { serverPort = 0
    , dbHost = "Error"
    , dbPort = 0
    , dbname = "Error"
    , dbUser = "Error"
    , dbPassword = "Error"
    , maxElem  = 0
    , priorityLevel = ERROR
    , logOutput = "cons"
    }

configuration :: Configuration              -- Try to read configuration file.
configuration =
  case getConfiguration "../config.json" of
    Right v -> v
    Left _  -> errorConfig

time :: IO String                             -- Get current time for the logger.
time = take 19 . show <$> getCurrentTime


connectInfo :: PS.ConnectInfo
connectInfo =
  PS.ConnectInfo
    { PS.connectHost = dbHost configuration
    , PS.connectPort = dbPort configuration
    , PS.connectDatabase = dbname configuration
    , PS.connectUser = dbUser configuration
    , PS.connectPassword = dbPassword configuration
    }

limitElem :: Int
limitElem = maxElem configuration

connectDB :: IO PS.Connection
connectDB = do 
  writingLine INFO "Sent a request to the database."
  writingLineDebug connectInfo
  PS.connect connectInfo


data Priority = DEBUG | INFO | WARNING | ERROR      -- Data type for the logger.
  deriving (Show, Eq, Ord, Generic, FromJSON)
  
file :: I.Handle                                  -- Get Handle for the logfile.
{-# NOINLINE file #-}
file  = unsafePerformIO $ I.openFile "../log.log" I.AppendMode

writingLine :: Priority -> String -> IO ()               -- Function writes log
writingLine lvl str =                                    --    information down.
  if lvl >= logLevel 
    then do
      t <- time
      let string = t ++ " UTC   " ++ fun lvl ++ " - " ++ str
      case out of
        "file" -> do
          I.hPutStrLn file string
          I.hFlush file
        _ -> putStrLn string
    else pure ()
 where
  out = logOutput configuration
  fun val = case val of
    DEBUG -> "DEBUG  "
    INFO -> "INFO   "
    WARNING -> "WARNING"
    ERROR -> "ERROR  "  
 
writingLineDebug :: (Show a) => a -> IO ()  
writingLineDebug s = writingLine DEBUG $ show s
  
logLevel :: Priority                                           -- Logging level.
logLevel =  priorityLevel configuration

port :: Int                                                  -- TCP port number.
port =  serverPort configuration

