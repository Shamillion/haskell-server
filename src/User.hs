{-# LANGUAGE OverloadedStrings #-}

module User where

import Crypto.KDF.BCrypt                (hashPassword)
import Data.Aeson                       (object, ToJSON, toJSON, (.=))
import qualified Data.ByteString.Char8  as BC 
import Data.Char                        (ord)
import Database.PostgreSQL.Simple       (close, query_)
import Database.PostgreSQL.Simple.Types (Query(..))
import qualified Data.Text as T 
import System.IO.Unsafe                 (unsafePerformIO)
import Config                           (connectDB, writingLineDebug)
import Lib                              (fromMaybe, readNum)


-- Creating a database query to get a list of users
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
  toJSON (User userId nameUser creationDate isAdmin isAuthor) =
    object
      [ "user_id"       .= userId
      , "name_user"     .= nameUser      
      , "creation_date" .= creationDate
      , "is_admin"      .= isAdmin
      , "is_author"     .= isAuthor
      ]   
 
errorUser :: User
errorUser = User 0 "error" "error" False False

parseUser :: [T.Text] -> User
parseUser ls
  | length ls /= 6 = errorUser
  | idUsr == 0 = errorUser
  | otherwise = User idUsr u2 u3 isAdm isAth
  where     
    [u1,u2,u3,u4,u5,_] = ls
    idUsr = readNum u1 
    isAdm = u4 == "t" || u4 == "true"
    isAth = u5 == "t" || u5 == "true"


-- Request example (strict order):  
-- '../user?name_user=Bob&login=Bob123&pass=11111&is_admin=false&is_author=true'   
createUser :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createUser False _ = "404"
createUser _ ls 
  | null ls || map fst ls /= checkList = "404"
  | searchNothing = "404"
  | not (checkUniqLogin login) = "406uu"
  | otherwise = Query $
      "INSERT INTO users (name_user, login, pass, \
      \       creation_date, is_admin, is_author) \
      \ VALUES ('" <> nameUser <> "', '" <> login <> "', '" <> pass' <> 
                "', NOW(), '" <> isAdmin <> "', '" <> isAuthor <> "');"
  where
    checkList = ["name_user", "login", "pass", "is_admin", "is_author"]
    sndList = map (fromMaybe . snd) ls
    searchNothing = "???" `elem` sndList
    [nameUser, login, pass, isAdmin, isAuthor] = map (fromMaybe . snd) ls
    pass' = cryptoPass (sum . map ord . BC.unpack $ nameUser) pass    


-- Create a bcrypt hash for a password.
cryptoPass :: Int -> BC.ByteString -> BC.ByteString
cryptoPass n str = 
  unsafePerformIO $ hashPassword (mod n 7 + 4) str 

-- Checking the uniqueness of the login in the database.
checkUniqLogin :: BC.ByteString -> Bool
checkUniqLogin str = unsafePerformIO $ do
  conn <- connectDB
  ls <- query_ conn $ Query $ "SELECT name_user FROM users \
                      \ WHERE login = '" <> str <> "';" :: IO [[BC.ByteString]]
  close conn
  writingLineDebug ls
  pure $ null ls                   

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
