module Environment where
import Config (Configuration (maxElem), readConfigFile, connectingParameters)
import qualified Database.PostgreSQL.Simple as PS


data Environment = Environment
  { limitElem :: Int,
    connectInfo :: PS.ConnectInfo,
    configuration :: Configuration
  }
  deriving (Show)

environment :: IO Environment
environment = do
  conf <- readConfigFile
  pure $
    Environment
      { limitElem = maxElem conf,
        connectInfo = connectingParameters conf,
        configuration = conf
      }