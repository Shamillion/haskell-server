{-# LANGUAGE OverloadedStrings #-}


module MigrationsDB where


import Database.PostgreSQL.Simple (close, execute_, query_, Query)
import qualified Data.Text        as T 
import Config                     (connectDB, Priority(..), writingLine, writingLineDebug) 


checkDB :: Int -> IO Int
checkDB num
  | num > 2 = writingLine ERROR "Error Database!" >> pure num
  | otherwise = do
      writingLine INFO "Checking Database..."
      conn <- connectDB
      writingLineDebug qry
      db <- query_ conn qry :: IO [[T.Text]] 
      writingLineDebug db  
      let k = all (`elem` concat db) ["users","news","category","photo"]
      _ <- if k 
             then do
               close conn
               writingLine INFO "Database is OK."
               pure num
             else do
               mapM_ (execute_ conn) lq
               writingLine INFO "Database has been created."
               close conn
               checkDB (num + 1)       
      writingLine DEBUG "Checking database has been successfully."
      pure num
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
