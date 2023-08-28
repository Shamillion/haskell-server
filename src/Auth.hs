module Auth where

import Config (Priority (ERROR), connectDB, writingLine, writingLineDebug)
import Control.Exception (catch, throwIO)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString.Char8 as BC
import Data.String (fromString)
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error (AuthError (..))
import qualified Network.Wai as W
import User (User (..), getUser)

-- Returns the user with the username and password from the request.
checkAuth :: W.Request -> IO User
checkAuth req =
  if null filteredLs
    then throwIO NoAuthorization
    else case decodeLogAndPass of
      Left err -> do
        writingLine ERROR err
        throwIO DecodeLoginAndPassError
      Right [login, password] -> do
        conn <- connectDB
        let qry = getUser $ Query $ "WHERE login = '" <> login <> "'"
        writingLineDebug qry
        userList <- query_ conn qry :: IO [User]
        close conn
        writingLineDebug userList
        checkPassword password userList
      _ -> throwIO NoAuthorization
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
noAuthorization :: a -> AuthError -> IO a
noAuthorization val err = do
  writingLineDebug err
  pure val

-- Returns the user ID.
authorID :: W.Request -> IO Query
authorID req = do
  let userId = fromString . show . user_id <$> checkAuth req
  catch userId $ noAuthorization "Null"

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> IO Bool
isAdmin req =
  catch (is_admin <$> checkAuth req) . noAuthorization $ False

-- Checks the user's ability to create news.
isAuthor :: W.Request -> IO Bool
isAuthor req =
  catch (is_author <$> checkAuth req) . noAuthorization $ False
