{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module User where

import Config (connectDB, writingLineDebug)
import Crypto.KDF.BCrypt (hashPassword)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error (Error (CommonError, LoginOccupied, ParseError), ParseError (ParseUserError))
import GHC.Generics (Generic)
import Lib (readNum)

-- Creating a database query to get a list of users
getUser :: Query -> Query
getUser str =
  "SELECT  user_id, name_user, (creation_date :: TEXT), is_admin, is_author, \
  \ pass FROM users "
    <> str
    <> ";"

getUserAsText :: Query -> Query
getUserAsText str =
  "SELECT  (user_id :: TEXT), name_user, \
  \ (creation_date :: TEXT), (is_admin :: TEXT), (is_author :: TEXT) \
  \ FROM users "
    <> str
    <> ";"

data User = User
  { user_id :: Int,
    name_user :: T.Text,
    creation_date' :: T.Text,
    is_admin :: Bool,
    is_author :: Bool,
    pass :: BC.ByteString
  }
  deriving (Show, Generic, FromRow)

instance ToJSON User where
  toJSON (User userId nameUser creationDate isAdmin isAuthor _) =
    object
      [ "user_id" .= userId,
        "name_user" .= nameUser,
        "creation_date" .= creationDate,
        "is_admin" .= isAdmin,
        "is_author" .= isAuthor
      ]

parseUser :: [T.Text] -> Either Error User
parseUser [userIdTxt, nameUser, creationDate, isAdmn, isAuthr] = do
  let idUsr = readNum userIdTxt
      isAdm = isAdmn `elem` ["t", "true"]
      isAth = isAuthr `elem` ["t", "true"]
  if idUsr == 0 
    then Left $ ParseError ParseUserError
    else pure $ User idUsr nameUser creationDate isAdm isAth ""
parseUser _ = Left $ ParseError ParseUserError      

-- Request example (strict order):
-- '../user?name_user=Bob&login=Bob123&pass=11111&is_admin=false&is_author=true'
createUser :: IO Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> IO (Either Error Query)
createUser isAdmn ls = do
  admin <- isAdmn
  if not admin || null ls || map fst ls /= checkList || searchNothing
    then pure $ Left CommonError
    else do
      uniq <- checkUniqLogin login
      if uniq
        then do
          password <- cryptoPass (sum . map ord . BC.unpack $ nameUser) pass
          pure . Right . Query $
            "INSERT INTO users (name_user, login, pass, \
            \       creation_date, is_admin, is_author) \
            \ VALUES ('"
              <> nameUser
              <> "', '"
              <> login
              <> "', '"
              <> password
              <> "', NOW(), '"
              <> isAdmin
              <> "', '"
              <> isAuthor
              <> "');"
        else pure $ Left LoginOccupied
  where
    checkList = ["name_user", "login", "pass", "is_admin", "is_author"]
    searchNothing = elem Nothing $ map snd ls
    [nameUser, login, pass, isAdmin, isAuthor] = map (fromMaybe "" . snd) ls

-- Disables the administrator rights of an automatically created user.
-- Request example '../user?block_admin=Adam'
blockAdminRights :: Bool -> Either Error Query
blockAdminRights False = Left CommonError
blockAdminRights _ =
  Right
    "UPDATE users SET is_admin = FALSE WHERE user_id = 99 AND login = 'Adam';"

-- Create a bcrypt hash for a password.
cryptoPass :: Int -> BC.ByteString -> IO BC.ByteString
cryptoPass num = hashPassword (mod num 7 + 4)

-- Checking the uniqueness of the login in the database.
checkUniqLogin :: BC.ByteString -> IO Bool
checkUniqLogin str = do
  conn <- connectDB
  ls <-
    query_ conn $
      Query $
        "SELECT name_user FROM users \
        \ WHERE login = '"
          <> str
          <> "';" ::
      IO [[BC.ByteString]]
  close conn
  writingLineDebug ls
  pure $ null ls
