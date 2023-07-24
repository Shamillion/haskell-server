module Photo where

import Config (Wrong (Wrong), connectDB)
import Data.ByteString.Base64.Lazy (decodeLenient)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (close, execute, query_)
import Database.PostgreSQL.Simple.Types (Query (..))

-- Creating a query to the database to get one photo.
getPhoto :: [(BC.ByteString, Maybe BC.ByteString)] -> Either Wrong Query
getPhoto [] = Left Wrong
getPhoto (x : _) =
  case x of
    (_, Nothing) -> Left Wrong
    (_, Just "") -> Left Wrong
    (_, Just n) ->
      if BC.all isDigit n
        then Right . Query $ "SELECT image FROM photo WHERE photo_id = " <> n <> ";"
        else Left Wrong

-- Decoding photos from Base64.
decodeImage :: [[T.Text]] -> LC.ByteString
decodeImage [] = "Error"
decodeImage ([img] : _) = decodeLenient . LC.fromStrict . encodeUtf8 $ img'
  where
    img' = T.drop 1 . T.dropWhile (/= ',') $ img
decodeImage (_ : _) = "Error"

-- The function sends the photo to the database and returns its ID in the table.
sendPhotoToDB :: BC.ByteString -> IO BC.ByteString
sendPhotoToDB str = do
  conn <- connectDB
  _ <- execute conn "INSERT INTO photo (image) VALUES (?);" [str]
  num <- query_ conn "SELECT max(photo_id) FROM photo;" :: IO [[Int]]
  close conn
  let [[num']] = num
  pure . BC.pack . show $ num'
