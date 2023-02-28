{-# LANGUAGE OverloadedStrings #-}

module Photo where

import Config (connectDB)
import Data.ByteString.Base64.Lazy (decodeLenient)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (close, execute, query_)
import Database.PostgreSQL.Simple.Types (Query (..))

-- Creating a query to the database to get one photo.
getPhoto :: [(BC.ByteString, Maybe BC.ByteString)] -> Query
getPhoto [] = "404"
getPhoto (x : _) = Query $
  case x of
    (_, Nothing) -> "404"
    (_, Just "") -> "404"
    (_, Just n) ->
      if BC.all isDigit n
        then "SELECT image FROM photo WHERE photo_id = " <> n <> ";"
        else "404"

-- Decoding photos from Base64.
decodeImage :: [[T.Text]] -> LC.ByteString
decodeImage [] = "404"
decodeImage ([img] : _) = decodeLenient . LC.fromStrict . encodeUtf8 $ img'
  where
    img' = T.drop 1 . T.dropWhile (/= ',') $ img
decodeImage (_ : _) = "404"

-- The function sends the photo to the database and returns its ID in the table.
sendPhotoToDB :: BC.ByteString -> IO BC.ByteString
sendPhotoToDB str = do
  conn <- connectDB
  _ <- execute conn "INSERT INTO photo (image) VALUES (?);" [str]
  num <- query_ conn "SELECT max(photo_id) FROM photo;" :: IO [[Int]]
  close conn
  let [[num']] = num
  pure . BC.pack . show $ num'
