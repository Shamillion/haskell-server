module Auth where

import Config (Priority (ERROR))
import ConnectDB (connectDB)
import Control.Exception (catch, throwIO)
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString.Char8 as BC
import Data.String (fromString)
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Environment (Environment)
import Error (AuthError (..))
import Logger (writingLine, writingLineDebug)
import qualified Network.Wai as W
import User (User (..), mkGetUserQuery)

-- Returns the user with the username and password from the request.
checkAuth :: W.Request -> ReaderT Environment IO User
checkAuth req =
  if null filteredLs
    then liftIO $ throwIO NoAuthorization
    else case decodeLogAndPass of
      Left err -> do
        writingLine ERROR err
        liftIO $ throwIO DecodeLoginAndPassError
      Right [login, password] -> do
        conn <- connectDB
        let qry = mkGetUserQuery $ Query $ "WHERE login = '" <> login <> "'"
        writingLineDebug qry
        userList <- liftIO $ query_ conn qry :: ReaderT Environment IO [User]
        liftIO $ close conn
        writingLineDebug userList
        liftIO $ checkPassword password userList
      _ -> liftIO $ throwIO NoAuthorization
  where
    filteredLs = filter ((== "Authorization") . fst) $ W.requestHeaders req
    [(_, str)] = filteredLs
    decodeLogAndPass = BC.split ':' <$> (BB.decode =<< (lastElem . BC.split ' ' $ str))
    lastElem [] = Left "Error! Empty list"
    lastElem arr = pure . (\(x : _) -> x) . reverse $ arr
    checkPassword _ [] = throwIO NoSuchUserInDB
    checkPassword password (user : _)
      | validatePassword password $ pass user = pure user
      | otherwise = throwIO NoSuchUserInDB

-- Returns the value in case of failed authorization.
noAuthorization :: Environment -> a -> AuthError -> IO a
noAuthorization env val err = do
  runReaderT (writingLineDebug err) env
  pure val

-- Returns the user ID.
authorID :: W.Request -> ReaderT Environment IO Query
authorID req = do
  env <- ask
  let userId = fromString . show . user_id <$> runReaderT (checkAuth req) env
  liftIO . catch userId . noAuthorization env $ "Null"

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> ReaderT Environment IO Bool
isAdmin req = do
  env <- ask
  let admin = is_admin <$> runReaderT (checkAuth req) env
  liftIO . catch admin . noAuthorization env $ False

-- Checks the user's ability to create news.
isAuthor :: W.Request -> ReaderT Environment IO Bool
isAuthor req = do
  env <- ask
  let author = is_author <$> runReaderT (checkAuth req) env
  liftIO . catch author . noAuthorization env $ False
