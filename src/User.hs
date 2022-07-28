{-# LANGUAGE OverloadedStrings #-}

module User where

import Crypto.KDF.BCrypt (hashPassword)
import Data.Aeson
import Data.Char                       (ord)
import Data.Monoid                     ((<>))
import Database.PostgreSQL.Simple
import qualified Data.Text as T 
import Database.PostgreSQL.Simple.Types 
import qualified Data.ByteString.Char8 as BC 
import System.IO.Unsafe                (unsafePerformIO)
import Config



getUser :: Query -> Query
getUser str = "SELECT  (user_id :: TEXT), name_user, \
   \ (creation_date :: TEXT), (is_admin :: TEXT), (is_author :: TEXT), pass \
   \ FROM users " <> str <> ";" 

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
  | length ls /= 6 = errorUser
  | otherwise = User id u2 u3 isAdm isAth
  where     
    [u1,u2,u3,u4,u5,u6] = ls
    id = read $ T.unpack u1 :: Int
    isAdm = u4 == "t" || u4 == "true"
    isAth = u5 == "t" || u5 == "true"


-- /user?name_user=Bob&login=Bob123&pass=11111&is_admin=false&is_author=true   (strict order) 
createUser :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createUser False _ = "404"
createUser _ ls 
  | ls == [] || (map fst ls) /= checkList = "404"
  | searchNothing = "404"
  | not (checkUniqLogin login) = "406uu"
  | otherwise = Query $
      "INSERT INTO users (name_user, login, pass, \
      \       creation_date, is_admin, is_author) \
      \ VALUES ('" <> name_user <> "', '" <> login <> "', '" <> pass' <> 
                "', NOW(), '" <> is_admin <> "', '" <> is_author <> "');"
  where
    checkList = ["name_user", "login", "pass", "is_admin", "is_author"]
    sndList = map (fromMaybe . snd) ls
    searchNothing = elem "Nothing" sndList
    [name_user, login, pass, is_admin, is_author] = map (fromMaybe . snd) ls
    pass' = cryptoPass (sum . map ord . BC.unpack $ name_user) pass    
    fromMaybe (Just e) = e
    fromMaybe Nothing  = "Nothing"


cryptoPass :: Int -> BC.ByteString -> BC.ByteString
cryptoPass n str = 
  unsafePerformIO $ hashPassword (mod n 7 + 4) str 


checkUniqLogin :: BC.ByteString -> Bool
checkUniqLogin str = unsafePerformIO $ do
  conn <- connectDB
  ls <- query_ conn $ Query $ "SELECT name_user FROM users \
                      \ WHERE login = '" <> str <> "';" :: IO [[BC.ByteString]]
  print ls
  pure $ ls == []                   

--passwordToHash :: Query          -- Turned passwords to hash in DB 
--passwordToHash = unsafePerformIO $ do
  --conn <- connectDB
  --ls <- query_ conn "SELECT  (user_id :: TEXT), name_user, pass FROM users;" :: IO [[BC.ByteString]]
  --let hp = map fun ls
      --qry (f,s) = "UPDATE users SET pass = '" <> s <> "' \
                     -- \ WHERE user_id = " <> f <> ";"
      --hp' = map (\p -> execute_ conn $ Query $ qry p) hp
  --x <- sequence hp'
  --print x    
  --pure "SELECT name_user FROM users;"
  --where
    --fun (x1:x2:x3:xs) = 
      --(x1, cryptoPass (sum $ map ord $ BC.unpack x2) x3) 
  

 -- SELECT user_id, name_user FROM users WHERE login = 'Logan';


