module ConnectDB where

import Config (Priority (INFO), writingLine, writingLineDebug)
import Control.Monad.Reader (ReaderT, asks, liftIO)
import qualified Database.PostgreSQL.Simple as PS
import Environment (Environment (connectInfo))

-- Establishing a connection to the database.
connectDB :: ReaderT Environment IO PS.Connection
connectDB = do
  connectInf <- asks connectInfo
  liftIO $ do
    writingLine INFO "Sent a request to the database."
    writingLineDebug connectInf
    PS.connect connectInf
