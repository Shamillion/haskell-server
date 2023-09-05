module Photo where

import Config (connectDB)
import Control.Exception (throwIO)
import qualified Data.Bifunctor as BF
import Data.ByteString.Base64.Lazy (decodeLenient)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (close, execute, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error (Error (CommonError, ParseError), ParseError (DecodeImageError))
import Lib (runGetQuery)
import qualified Network.Wai as W

-- Creating a query to the database to get one photo.
mkGetPhotoQuery :: [(BC.ByteString, Maybe BC.ByteString)] -> Either Error Query
mkGetPhotoQuery [] = Left CommonError
mkGetPhotoQuery (x : _) =
  case x of
    (_, Nothing) -> Left CommonError
    (_, Just "") -> Left CommonError
    (_, Just numStr) ->
      if BC.all isDigit numStr
        then Right . Query $ "SELECT image FROM photo WHERE photo_id = " <> numStr <> ";"
        else Left CommonError

-- Decoding photos from Base64.
decodeImage :: [[T.Text]] -> Either Error LC.ByteString
decodeImage [] = Left $ ParseError DecodeImageError
decodeImage ([txt] : _) = pure $ header <> ";" <> image
  where
    clearedTxt = T.drop 1 . T.dropWhile (/= ',') $ txt
    image = decodeLenient . LC.fromStrict . encodeUtf8 $ clearedTxt
    header = LC.fromStrict . encodeUtf8 . T.drop 1 . T.takeWhile (/= ';') . T.dropWhile (/= ':') $ txt
decodeImage (_ : _) = Left $ ParseError DecodeImageError

-- The function sends the photo to the database and returns its ID in the table.
sendPhotoToDB :: BC.ByteString -> IO BC.ByteString
sendPhotoToDB str = do
  conn <- connectDB
  _ <- execute conn "INSERT INTO photo (image) VALUES (?);" [str]
  [[val]] <- query_ conn "SELECT (max(photo_id) :: varchar) FROM photo;" :: IO [[BC.ByteString]]
  close conn
  pure val

getPhotoHandler :: W.Request -> IO LC.ByteString
getPhotoHandler req = do
  queryPhoto <- buildGetPhotoQuery req
  photoTxt <- runGetQuery queryPhoto
  encodePhoto photoTxt

buildGetPhotoQuery :: W.Request -> IO Query
buildGetPhotoQuery req = do
  let eitherQuery = mkGetPhotoQuery $ W.queryString req
  case eitherQuery of
    Left err -> throwIO err
    Right qry -> pure qry

encodePhoto :: [[T.Text]] -> IO LC.ByteString
encodePhoto ls =
  case decodeImage ls of
    Left err -> throwIO err
    Right photo -> pure photo

headerAndImage :: LC.ByteString -> (BC.ByteString, LC.ByteString)
headerAndImage = BF.first LC.toStrict . fmap (LC.drop 1) . LC.span (/= ';')
