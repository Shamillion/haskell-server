module Environment where

import Config (Configuration (dbHost, dbPassword, dbPort, dbUser, dbname), readConfigFile)
import qualified Database.PostgreSQL.Simple as PS

data Environment = Environment
  { configuration :: Configuration,
    connectInfo :: PS.ConnectInfo
  }
  deriving (Show)

environment :: IO Environment
environment = do
  conf <- readConfigFile
  pure $
    Environment
      { configuration = conf,
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
