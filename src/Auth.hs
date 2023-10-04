{-# LANGUAGE LambdaCase #-}

module Auth where

import Config (Priority (ERROR))
import ConnectDB (connectDB)
import Control.Monad.Catch (catch, handle)
import Control.Monad.Reader (liftIO)
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
      | otherwise = throwError $ AuthError InvalidPassword

-- Returns the user ID.
authorID :: W.Request -> Flow Query
authorID req =
  handle (pure . const "Null" :: Error -> Flow Query) $
    fromString . show . user_id <$> checkAuth req

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> Flow Bool
isAdmin req =
  handle (pure . const False :: Error -> Flow Bool) $
    is_admin <$> checkAuth req

-- Checks the user's ability to create news.
isAuthor :: W.Request -> Flow Bool
isAuthor req = do
  let isAuthr = is_author <$> checkAuth req
  catch isAuthr $
    \case
      AuthError InvalidPassword -> isAuthr
      _ -> pure False
