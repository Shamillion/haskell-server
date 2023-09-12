module ConnectDB where

import Config (Priority (INFO))
import Control.Monad.Reader (ReaderT, asks, liftIO)
import qualified Database.PostgreSQL.Simple as PS
import Environment (Environment (connectInfo))
import Logger (writingLine, writingLineDebug)

-- Establishing a connection to the database.
connectDB :: ReaderT Environment IO PS.Connection
connectDB = do
  connectInf <- asks connectInfo
  writingLine INFO "Sent a request to the database."
  writingLineDebug connectInf
  liftIO $ PS.connect connectInf
