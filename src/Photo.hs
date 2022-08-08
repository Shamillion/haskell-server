{-# LANGUAGE OverloadedStrings #-}


module Photo where

import           Data.Char                  (isDigit)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T 
import           Data.Text.Encoding        (encodeUtf8)
import           Data.ByteString.Base64.Lazy    (decodeLenient)
import           Data.Monoid                     ((<>))
import System.IO.Unsafe          (unsafePerformIO)
import Config






getPhoto :: [(BC.ByteString, Maybe BC.ByteString)] -> Query
getPhoto [] = "404"
getPhoto (x:xs) = Query $
  case x of
  (_, Nothing) -> "404"
  (_, Just "") -> "404"
  (_, Just n)  -> 
    if BC.all isDigit n
      then "SELECT image FROM photo WHERE photo_id = " <> n <> ";"
      else "404"


decodeImage :: [[T.Text]] -> LC.ByteString
decodeImage [] = "404"
decodeImage [[img]] = decodeLenient . LC.fromStrict . encodeUtf8 $ img'
  where
    img' = T.drop 1 . T.dropWhile (/=',') $ img
    
    
sendPhotoToDB :: BC.ByteString -> BC.ByteString
sendPhotoToDB str = unsafePerformIO $ do
  conn <- connectDB
  val <- execute conn "INSERT INTO photo (image) VALUES (?);" [str]
  num <- query_ conn "SELECT max(photo_id) FROM photo;" :: IO [[Int]]
  let [[num']] = num
  pure . BC.pack . show $ num' 





