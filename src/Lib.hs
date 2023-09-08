module Lib where

import Config (limitElem, writingLineDebug)
import ConnectDB (connectDB)
import Control.Monad.Reader (ReaderT, liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.List as LT
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, Query, close, execute_, query_)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Environment (Environment)
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

newtype LimitAndOffsetHandle m = LimitAndOffsetHandle
  { limitElemH :: m Int
  }

limitAndOffsetHandler :: LimitAndOffsetHandle IO
limitAndOffsetHandler =
  LimitAndOffsetHandle
    { limitElemH = limitElem
    }

setLimitAndOffset ::
  Monad m =>
  LimitAndOffsetHandle m ->
  [(T.Text, Maybe T.Text)] ->
  m Query
setLimitAndOffset LimitAndOffsetHandle {..} ls = do
  limitInFile <- limitElemH
  let limit = listToValue "limit" setLimit limitInFile
      setLimit val = if val > 0 && val < limitInFile then val else limitInFile
  pure . Query $ " LIMIT " <> limit <> " OFFSET " <> offset
  where
    offset = listToValue "offset" (max 0) 0
    getMaybe param func =
      (LT.find ((== param) . fst) ls >>= snd >>= readMaybe . T.unpack) <&> func
    listToValue param func defaultVal = toByteString $
      case getMaybe param func of
        Just val -> val
        _ -> defaultVal
    toByteString = BC.pack . show

runGetQuery :: (Show r, FromRow r) => Query -> ReaderT Environment IO [r]
runGetQuery qry = do
  conn <- connectDB
  liftIO $ do
    dataFromDB <- query_ conn qry
    writingLineDebug dataFromDB
    close conn
    pure dataFromDB

runPostOrPutQuery :: Query -> ReaderT Environment IO Int64
runPostOrPutQuery qry = do
  conn <- connectDB
  liftIO $ do
    num <- execute_ conn qry
    close conn
    writingLineDebug num
    pure num

-- A comment for the user about adding or editing objects in the database.
mkComment :: Int64 -> LC.ByteString
mkComment num = LC.pack (show num) <> " position(s) done."

createAndEditObjectsHandler :: (W.Request -> ReaderT Environment IO Query) -> W.Request -> ReaderT Environment IO LC.ByteString
createAndEditObjectsHandler func req = do
  queryForDB <- func req
  num <- runPostOrPutQuery queryForDB
  pure $ mkComment num
