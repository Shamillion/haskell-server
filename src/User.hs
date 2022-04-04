{-# LANGUAGE OverloadedStrings #-}

module User where

import Data.Aeson
import Database.PostgreSQL.Simple
import qualified Data.Text as T hiding (last)






getUser :: Query
getUser = "SELECT  (user_id :: TEXT), name_user, (creation_date :: TEXT), \
   \ (is_admin :: TEXT), (is_author :: TEXT) \
   \ FROM users;"

data User = User
  { user_id        :: Int
  , name_user      :: T.Text
  , creation_date' :: T.Text
  , is_admin       :: Bool
  , is_author      :: Bool
  } 
   deriving Show

instance ToJSON User where
  toJSON (User user_id name_user creation_date' is_admin is_author) =
    object
      [ "user_id"       .= user_id
      , "name_user"     .= name_user      
      , "creation_date" .= creation_date'
      , "is_admin"      .= is_admin
      , "is_author"     .= is_author
      ]   
 
errorUser :: User
errorUser = User 0 "error" "error" False False

parseUser :: [T.Text] -> User
parseUser ls
  | length ls /= 5 = errorUser
  | otherwise = User id u2 u3 isAdm isAth
  where     
    [u1,u2,u3,u4,u5] = ls
    id = read $ T.unpack u1 :: Int
    isAdm = u4 == "t"
    isAth = u5 == "t"











