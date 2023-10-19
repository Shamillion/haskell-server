{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module User where

import ConnectDB (connectDB)
import Control.Monad.Reader (liftIO)
import Crypto.KDF.BCrypt (hashPassword)
import Data.Aeson (ToJSON, encode, object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Environment (Flow)
import Error (Error (CommonError, LoginOccupied, ParseError), ParseError (ParseUserError), throwError)
import GHC.Generics (Generic)
import Lib (readNum, runGetQuery, setLimitAndOffset)
import Logger (writingLineDebug)
import Network.HTTP.Types (queryToQueryText)
import qualified Network.Wai as W

-- Creating a database query to get a list of users
mkGetUserQuery :: Query -> Query
mkGetUserQuery limitOffset =
  "SELECT  user_id, name_user, (creation_date :: TEXT), is_admin, is_author, \
  \ pass FROM users "
    <> limitOffset
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
buildCreateUserQuery :: Bool -> W.Request -> Flow Query
buildCreateUserQuery isAdmn req = do
  if not isAdmn || null dataFromRequest || map fst dataFromRequest /= checkList || searchNothing
    then throwError CommonError
    else do
      uniq <- checkUniqLogin login
      if uniq
        then do
          password <- liftIO $ cryptoPass (sum . map ord . BC.unpack $ nameUser) pass
          pure . Query $
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
        else throwError LoginOccupied
  where
    dataFromRequest = W.queryString req
    checkList = ["name_user", "login", "pass", "is_admin", "is_author"]
    searchNothing = elem Nothing $ map snd dataFromRequest
    [nameUser, login, pass, isAdmin, isAuthor] = map (fromMaybe "" . snd) dataFromRequest

-- Disables the administrator rights of an automatically created user.
-- Request example '../user?block_admin=Adam'
mkBlockAdminRightsQuery :: Bool -> Either Error Query
mkBlockAdminRightsQuery False = Left CommonError
mkBlockAdminRightsQuery _ =
  Right
    "UPDATE users SET is_admin = FALSE, is_author = FALSE WHERE user_id = 99 AND login = 'Adam';"

-- Create a bcrypt hash for a password.
cryptoPass :: Int -> BC.ByteString -> IO BC.ByteString
cryptoPass num = hashPassword (mod num 7 + 4)

-- Checking the uniqueness of the login in the database.
checkUniqLogin :: BC.ByteString -> Flow Bool
checkUniqLogin login = do
  conn <- connectDB
  usernameLs <-
    liftIO $
      query_ conn $
        Query $
          "SELECT name_user FROM users \
          \ WHERE login = '"
            <> login
            <> "';" ::
      Flow [[BC.ByteString]]
  liftIO $ close conn
  writingLineDebug usernameLs
  pure $ null usernameLs

getUserHandler :: W.Request -> Flow LC.ByteString
getUserHandler req = do
  queryUser <- buildGetUserQuery req
  user <- runGetQuery queryUser :: Flow [User]
  pure $ encode user

buildGetUserQuery :: W.Request -> Flow Query
buildGetUserQuery req = mkGetUserQuery <$> limitOffset
  where
    dataFromRequest = W.queryString req
    limitOffset = setLimitAndOffset $ queryToQueryText dataFromRequest

buildEditUserQuery :: Bool -> W.Request -> Flow Query
buildEditUserQuery isAdmin _ =
  case mkBlockAdminRightsQuery isAdmin of
    Left err -> throwError err
    Right qry -> pure qry
