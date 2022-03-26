{-# LANGUAGE OverloadedStrings #-}

module Author where

import Data.Aeson
import Database.PostgreSQL.Simple
import qualified Data.Text as T hiding (last)






getAuthor :: Query
getAuthor = "SELECT  (author_id :: TEXT), name_user, surname_user, avatar, \
   \  (creation_date :: TEXT), (is_admin :: TEXT), description_author \
   \ FROM author \   
   \ INNER JOIN  users ON users.user_id = author.user_id;"

data Author = Author
  { author_id      :: Int
  , name_user      :: T.Text
  , surname_user   :: T.Text
  , avatar         :: T.Text
  , creation_date' :: T.Text
  , is_admin       :: Bool
  , description_author :: T.Text
  } 
   deriving Show

instance ToJSON Author where
  toJSON (Author author_id name_user surname_user avatar 
                 creation_date' is_admin description_author) =
    object
      [ "author_id"     .= author_id
      , "name_user"     .= name_user
      , "surname_user"  .= surname_user
      , "avatar"        .= avatar
      , "creation_date" .= creation_date'
      , "is_admin"      .= is_admin
      , "description_author" .= description_author
      ]   
 
errorAuthor :: Author
errorAuthor = Author 0 "error" "error" "error" "error" False "error"

parseAuthor :: [T.Text] -> Author
parseAuthor ls
  | length ls /= 7 = errorAuthor
  | otherwise = Author id a2 a3 a4 a5 isAdm a7
  where     
    [a1,a2,a3,a4,a5,a6,a7] = ls
    id = read $ T.unpack a1 :: Int
    isAdm = a6 == "t"












