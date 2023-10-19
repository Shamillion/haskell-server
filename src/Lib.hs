module Lib where

import Config (Configuration (maxElem))
import ConnectDB (connectDB)
import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, asks, liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.List as LT
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, Query, close, execute_, query_)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Environment (Environment, Flow, configuration)
import Error (Error (DatabaseError), throwError)
import Logger (writingLineDebug)
import qualified Network.Wai as W
import Text.Read (readMaybe)

readNum :: T.Text -> Int
readNum n =
  case readMaybe $ T.unpack n of
    Just int -> int
    _ -> 0

initTxt :: T.Text -> T.Text
initTxt "" = ""
initTxt txt = T.init txt

tailTxt :: T.Text -> T.Text
tailTxt "" = ""
tailTxt txt = T.tail txt

splitOnTxt :: T.Text -> T.Text -> [T.Text]
splitOnTxt _ "" = []
splitOnTxt c txt = T.splitOn c txt

setLimitAndOffset ::
  Monad m =>
  [(T.Text, Maybe T.Text)] ->
  ReaderT Environment m Query
setLimitAndOffset dataFromRequest = do
  limitInConfig <- asks (maxElem . configuration)
  let limit = listToValue "limit" setLimit limitInConfig
      setLimit val = if val > 0 && val < limitInConfig then val else limitInConfig
  pure . Query $ " LIMIT " <> limit <> " OFFSET " <> offset
  where
    offset = listToValue "offset" (max 0) 0
    getMaybe param func =
      (LT.find ((== param) . fst) dataFromRequest >>= snd >>= readMaybe . T.unpack) <&> func
    listToValue param func defaultVal = toByteString $
      case getMaybe param func of
        Just val -> val
        _ -> defaultVal
    toByteString = BC.pack . show

runGetQuery :: (Show r, FromRow r) => Query -> Flow [r]
runGetQuery qry = do
  conn <- connectDB
  dataFromDB <- liftIO $ query_ conn qry
  writingLineDebug dataFromDB
  liftIO $ close conn
  pure dataFromDB

runPostOrPutQuery :: Query -> Flow Int64
runPostOrPutQuery qry = do
  conn <- connectDB
  eitherNum <- liftIO . try . execute_ conn $ qry :: Flow (Either SomeException Int64)
  liftIO $ close conn
  writingLineDebug eitherNum
  case eitherNum of
    Left _ -> throwError DatabaseError
    Right num -> pure num

-- A comment for the user about adding or editing objects in the database.
mkComment :: Int64 -> LC.ByteString
mkComment num = LC.pack (show num) <> " position(s) done."

createAndEditObjectsHandler :: (W.Request -> Flow Query) -> W.Request -> Flow LC.ByteString
createAndEditObjectsHandler func req = do
  queryForDB <- func req
  num <- runPostOrPutQuery queryForDB
  pure $ mkComment num
