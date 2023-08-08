module Lib where

import Config (connectDB, limitElem, writingLineDebug)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.List as LT
import Data.String (IsString)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Query, close, execute_, query_)
import Database.PostgreSQL.Simple.Types (Query (Query))
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

-- Pulls a value from a list of lists.
drawOut :: Data.String.IsString a => [[a]] -> a
drawOut [] = ""
drawOut ([] : _) = ""
drawOut ([x] : _) = x
drawOut (_ : _) = ""

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

runGetQuery :: Query -> IO [[T.Text]]
runGetQuery qry = do
  conn <- connectDB
  dataFromDB <- query_ conn qry :: IO [[T.Text]]
  writingLineDebug dataFromDB
  close conn
  pure dataFromDB

runPostOrPutQuery :: Query -> IO Int64
runPostOrPutQuery qry = do
  conn <- connectDB
  num <- execute_ conn qry
  close conn
  writingLineDebug num
  pure num

sendComment :: Int64 -> IO LC.ByteString
sendComment num = pure $ LC.pack (show num) <> " position(s) done."

createAndEditHandler :: (W.Request -> IO Query) -> W.Request -> IO LC.ByteString
createAndEditHandler func req = do
  queryForDB <- func req
  num <- runPostOrPutQuery queryForDB
  sendComment num
