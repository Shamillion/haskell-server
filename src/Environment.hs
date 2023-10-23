module Environment where

import Config (Configuration (dbHost, dbPassword, dbPort, dbUser, dbname, logOutput, maxElem, priorityLevel), Priority, readConfigFile)
import Control.Monad.Reader (ReaderT)
import qualified Database.PostgreSQL.Simple as PS

data Environment = Environment
  { limitElem :: Int,
    loggingLevel :: Priority,
    logOutputObject :: String,
    connectInfo :: PS.ConnectInfo
  }
  deriving (Show)

buildEnvironment :: IO Environment
buildEnvironment = do
  conf <- readConfigFile
  pure $
    Environment
      { limitElem = maxElem conf,
        loggingLevel = priorityLevel conf,
        logOutputObject = logOutput conf,
        connectInfo = connectingParameters conf
      }

-- Parameters for connecting to the database.
connectingParameters :: Configuration -> PS.ConnectInfo
connectingParameters conf = do
  PS.ConnectInfo
    { PS.connectHost = dbHost conf,
      PS.connectPort = dbPort conf,
      PS.connectDatabase = dbname conf,
      PS.connectUser = dbUser conf,
      PS.connectPassword = dbPassword conf
    }

type Flow = ReaderT Environment IO
