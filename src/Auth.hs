module Auth where

import Config (Priority (ERROR))
import ConnectDB (connectDB)
import Control.Exception (catch)
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString.Char8 as BC
import Data.String (fromString)
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Environment (Flow)
import Error (AuthError (..), Error (AuthError), throwError)
import Logger (writingLine, writingLineDebug)
import qualified Network.Wai as W
import User (User (..), mkGetUserQuery)

-- Returns the user with the username and password from the request.
checkAuth :: W.Request -> Flow User
checkAuth req =
  if null filteredLs
    then throwError $ AuthError NoAuthorization
    else case decodeLogAndPass of
      Left err -> do
        writingLine ERROR err
        throwError $ AuthError DecodeLoginAndPassError
      Right [login, password] -> do
        conn <- connectDB
        let qry = mkGetUserQuery $ Query $ "WHERE login = '" <> login <> "'"
        writingLineDebug qry
        userList <- liftIO $ query_ conn qry :: Flow [User]
        liftIO $ close conn
        writingLineDebug userList
        checkPassword password userList
      _ -> throwError $ AuthError NoAuthorization
  where
    filteredLs = filter ((== "Authorization") . fst) $ W.requestHeaders req
    [(_, str)] = filteredLs
    decodeLogAndPass = BC.split ':' <$> (BB.decode =<< (lastElem . BC.split ' ' $ str))
    lastElem [] = Left "Error! Empty list"
    lastElem arr = pure . (\(x : _) -> x) . reverse $ arr
    checkPassword _ [] = throwError $ AuthError NoSuchUserInDB
    checkPassword password (user : _)
      | validatePassword password $ pass user = pure user
      | otherwise = throwError $ AuthError NoSuchUserInDB

-- Returns the user ID.
authorID :: W.Request -> Flow Query
authorID req = do
  env <- ask
  let userId = fromString . show . user_id <$> runReaderT (checkAuth req) env
  liftIO $ catch userId (pure . const "Null" :: Error -> IO Query)

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> Flow Bool
isAdmin req = do
  env <- ask
  let admin = is_admin <$> runReaderT (checkAuth req) env
  liftIO $ catch admin (pure . const False :: Error -> IO Bool)

-- Checks the user's ability to create news.
isAuthor :: W.Request -> Flow Bool
isAuthor req = do
  env <- ask
  let author = is_author <$> runReaderT (checkAuth req) env
  liftIO $ catch author (pure . const False :: Error -> IO Bool)
