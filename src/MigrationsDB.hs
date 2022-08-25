{-# LANGUAGE OverloadedStrings #-}


module MigrationsDB where

--import           Data.Char                  (isDigit)
import Database.PostgreSQL.Simple
--import Database.PostgreSQL.Simple.Types 
--import qualified Data.ByteString.Char8 as BC 
--import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T 
--import           Data.Text.Encoding        (encodeUtf8)
--import           Data.ByteString.Base64.Lazy    (decodeLenient)
--import           Data.Monoid                     ((<>))
--import System.IO.Unsafe                           (unsafePerformIO)
import Config


checkDB :: Int -> IO ()
checkDB num
  | num > 2 = writingLine ERROR "Error Database!" 
  | otherwise = do
      writingLine INFO "Checking Database..."
      conn <- connectDB
      writingLineDebug qry
      db <- query_ conn qry :: IO [[T.Text]] 
      writingLineDebug db  
      let k = all (`elem` (concat db)) ["users","news","category","photo"]
      if k 
        then do
          close conn
          writingLine INFO "Database is OK."
        else do
         -- mapM_ (execute_ conn) lq
          writingLine INFO "Database has been created."
          close conn
          checkDB (num + 1)       
      writingLine DEBUG "Checking database has been successfully."
      where
        qry = "SELECT table_name FROM information_schema.tables \
              \ WHERE table_schema NOT IN ('information_schema','pg_catalog');"
        lq = [createTableUsers, createTableCategory, 
                                    createTableNews, createTablePhoto]



createTableUsers :: Query
createTableUsers =
  "CREATE TABLE users( \
  \ user_id SERIAL PRIMARY KEY, \
  \ name_user VARCHAR(30), \
  \ login VARCHAR(30), \
  \ pass TEXT, \
  \ creation_date DATE, \
  \ is_admin  BOOLEAN, \
  \ is_author BOOLEAN );" 



createTableCategory :: Query
createTableCategory = 
  "CREATE TABLE category( \
  \ category_id SERIAL PRIMARY KEY, \
  \ name_category VARCHAR(100), \
  \ parent_category VARCHAR(100) );"


createTableNews :: Query
createTableNews = 
  "CREATE TABLE news( \
  \ news_id SERIAL PRIMARY KEY, \
  \ title TEXT, \
  \ creation_date DATE, \
  \ user_id INT NOT NULL, \
  \ category_id INT NOT NULL, \
  \ content TEXT, \
  \ photo INT[], \
  \ is_published  BOOLEAN, \
  \ FOREIGN KEY (user_id) REFERENCES users (user_id), \
  \ FOREIGN KEY (category_id) REFERENCES category (category_id) );"

createTablePhoto :: Query
createTablePhoto = 
  "CREATE TABLE photo(photo_id SERIAL PRIMARY KEY, image TEXT);"
