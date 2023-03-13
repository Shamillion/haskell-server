module MigrationsDB where

import Config (Priority (..), connectDB, writingLine, writingLineDebug)
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, execute_, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import User (cryptoPass)


-- Checking the availability of the necessary tables in the database.
checkDB :: Int -> IO Int
checkDB num
  | num > 2 = writingLine ERROR "Error Database!" >> pure num
  | otherwise = do
    writingLine INFO "Checking Database..."
    conn <- connectDB
    writingLineDebug qry
    db <- query_ conn qry :: IO [[T.Text]]
    writingLineDebug db
    let k = all (`elem` concat db) ["users", "news", "category", "photo"]
    _ <-
      if k
        then do
          close conn
          writingLine INFO "Database is OK."
          pure num
        else do
          addAdmin' <- addAdmin
          mapM_ (execute_ conn) 
            [ createTableUsers,
              addAdmin',
              createTableCategory,
              createTableNews,
              createTablePhoto
            ]
          writingLine INFO "Database has been created."
          close conn
          checkDB (num + 1)
    writingLine DEBUG "Checking database has been successfully."
    pure num
  where
    qry =
      "SELECT table_name FROM information_schema.tables \
      \ WHERE table_schema NOT IN ('information_schema','pg_catalog');"


createTableUsers :: Query
createTableUsers =
  "CREATE TABLE IF NOT EXISTS users( \
  \ user_id SERIAL PRIMARY KEY, \
  \ name_user VARCHAR(30), \
  \ login VARCHAR(30), \
  \ pass TEXT, \
  \ creation_date DATE, \
  \ is_admin  BOOLEAN, \
  \ is_author BOOLEAN );"

-- Creating the first administrator. Required to create other users.
addAdmin :: IO Query
addAdmin = do
  pass' <- cryptoPass (sum . map ord . BC.unpack $ "Adam") "sixthDay"
  pure . Query $
    "INSERT INTO users \
    \ (user_id, name_user, login, pass, creation_date, is_admin, is_author) \
    \ VALUES (99, 'Adam', 'Adam', '"
      <> pass'
      <> "', NOW(), TRUE, FALSE) \
         \ ON CONFLICT DO NOTHING;"

createTableCategory :: Query
createTableCategory =
  "CREATE TABLE IF NOT EXISTS category( \
  \ category_id SERIAL PRIMARY KEY, \
  \ name_category VARCHAR(100), \
  \ parent_category VARCHAR(100) );"

createTableNews :: Query
createTableNews =
  "CREATE TABLE IF NOT EXISTS news( \
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
  "CREATE TABLE IF NOT EXISTS photo(photo_id SERIAL PRIMARY KEY, image TEXT);"
