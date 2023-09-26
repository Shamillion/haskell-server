module ConnectDB where

import Config (Priority (INFO))
import Control.Monad.Reader (asks, liftIO)
import qualified Database.PostgreSQL.Simple as PS
import Environment (Environment (connectInfo), Flow)
import Logger (writingLine, writingLineDebug)

connectDB :: Flow PS.Connection
connectDB = do
  connectInf <- asks connectInfo
  writingLine INFO "Sent a request to the database."
  writingLineDebug connectInf
  liftIO $ PS.connect connectInf
