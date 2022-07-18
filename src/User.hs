{-# LANGUAGE OverloadedStrings #-}

module User where

import Data.Aeson
import Data.Monoid                     ((<>))
import Database.PostgreSQL.Simple
import qualified Data.Text as T 
import Database.PostgreSQL.Simple.Types 
import qualified Data.ByteString.Char8 as BC 




getUser :: Query -> Query
getUser limitOffset = "SELECT  (user_id :: TEXT), name_user, \
   \ (creation_date :: TEXT), (is_admin :: TEXT), (is_author :: TEXT) \
   \ FROM users " <> limitOffset <> ";" 

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
    isAdm = u4 == "t" || u4 == "true"
    isAth = u5 == "t" || u5 == "true"


-- /user?name_user=Bob&login=Bob123&pass=11111&is_admin=false&is_author=true   (strict order) 
createUser :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createUser False _ = "404"
createUser _ ls 
  | ls == [] || (map fst ls) /= checkList = "404"
  | searchNothing = "404"
  | otherwise = Query $
      "INSERT INTO users (name_user, login, pass, \
      \       creation_date, is_admin, is_author) \
      \ VALUES ('" <> name_user <> "', '" <> login <> "', '" <> pass <> 
                "', NOW(), '" <> is_admin <> "', '" <> is_author <> "');"
  where
    checkList = ["name_user", "login", "pass", "is_admin", "is_author"]
    sndList = map (fromMaybe . snd) ls
    searchNothing = elem "Nothing" sndList
    [name_user, login, pass, is_admin, is_author] = map (fromMaybe . snd) ls
    fromMaybe (Just e) = e
    fromMaybe Nothing  = "Nothing"









