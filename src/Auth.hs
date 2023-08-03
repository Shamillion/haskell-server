module Auth where

import Config (Priority (ERROR), connectDB, writingLine, writingLineDebug)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString.Char8 as BC
import Data.String (fromString)
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Network.Wai as W
import User (User (..), getUser)

-- Returns the user with the username and password from the request.
checkAuth :: RequestHeaders -> IO (Either String User)
checkAuth ls =
  if null filteredLs
    then pure $ Left "No Authorization"
    else case decodeLogAndPass of
      Left err -> do
        writingLine ERROR err
        pure $ Left err
      Right [login, password] -> do
        conn <- connectDB
        let qry = getUser $ Query $ "WHERE login = '" <> login <> "'"
        writingLineDebug qry
        userList <- query_ conn qry :: IO [User]
        close conn
        writingLineDebug userList
        pure $ checkPassword password userList
      _ -> pure $ Left "No Authorization"
  where
    filteredLs = filter ((== "Authorization") . fst) ls
    [(_, str)] = filteredLs
    decodeLogAndPass = BC.split ':' <$> (BB.decode =<< (lastElem . BC.split ' ' $ str))
    lastElem [] = Left "Error! Empty list"
    lastElem arr = pure . (\(x : _) -> x) . reverse $ arr
    checkPassword _ [] = Left "No such user in DB"
    checkPassword password (user : _)
      | validatePassword password $ pass user = Right user
      | otherwise = Left "No such user in DB"

-- Returns the user ID.
authorID :: W.Request -> IO Query
authorID req = do
  checkAuth' <- checkAuth (W.requestHeaders req)
  pure $ case checkAuth' of
    Right user -> fromString $ show $ user_id user
    _ -> "Null"

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> IO Bool
isAdmin req = do
  checkAuth' <- checkAuth (W.requestHeaders req)
  pure $ case checkAuth' of
    Right user -> is_admin user
    _ -> False

-- Checks the user's ability to create news.
isAuthor :: W.Request -> IO Bool
isAuthor req = do
  checkAuth' <- checkAuth (W.requestHeaders req)
  pure $ case checkAuth' of
    Right user -> is_author user
    _ -> False
