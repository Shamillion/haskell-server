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
import Error (Error (CommonError), ParseError (DecodeImageError))

-- Creating a query to the database to get one photo.
getPhoto :: [(BC.ByteString, Maybe BC.ByteString)] -> Either Error Query
getPhoto [] = Left CommonError
getPhoto (x : _) =
  case x of
    (_, Nothing) -> Left CommonError
    (_, Just "") -> Left CommonError
    (_, Just n) ->
      if BC.all isDigit n
        then Right . Query $ "SELECT image FROM photo WHERE photo_id = " <> n <> ";"
        else Left CommonError

-- Decoding photos from Base64.
decodeImage :: [[T.Text]] -> Either ParseError LC.ByteString
decodeImage [] = Left DecodeImageError
decodeImage ([img] : _) = pure . decodeLenient . LC.fromStrict . encodeUtf8 $ img'
  where
    img' = T.drop 1 . T.dropWhile (/= ',') $ img
decodeImage (_ : _) = Left DecodeImageError

-- The function sends the photo to the database and returns its ID in the table.
sendPhotoToDB :: BC.ByteString -> IO BC.ByteString
sendPhotoToDB str = do
  conn <- connectDB
  _ <- execute conn "INSERT INTO photo (image) VALUES (?);" [str]
  num <- query_ conn "SELECT max(photo_id) FROM photo;" :: IO [[Int]]
  close conn
  let [[num']] = num
  pure . BC.pack . show $ num'
