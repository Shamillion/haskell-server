module Auth where

import Config (Priority (ERROR), connectDB, writingLine, writingLineDebug)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString.Char8 as BC
import Data.String (fromString)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Network.Wai as W
import User (User (..), getUser, parseUser)

-- Returns the user with the username and password from the request.
checkAuth :: RequestHeaders -> IO (Either String User)
checkAuth ls =
  if null fls
    then pure $ Left "No Authorization"
    else case decodeLogAndPass of
      Left x -> do
        writingLine ERROR x
        pure $ Left x
      Right [x, y] -> do
        conn <- connectDB
        let qry = getUser $ Query $ "WHERE login = '" <> x <> "'"
        writingLineDebug qry
        userList <- query_ conn qry :: IO [[T.Text]]
        close conn
        writingLineDebug userList
        pure . checkPassword y . map parseUser $ userList
      _ -> pure $ Left "No Authorization"
  where
    fls = filter ((== "Authorization") . fst) ls
    [(_, str)] = fls
    decodeLogAndPass = BC.split ':' <$> (BB.decode =<< (lastElem . BC.split ' ' $ str))
    lastElem [] = Left "Error! Empty list"
    lastElem arr = pure . (\(x : _) -> x) . reverse $ arr
    checkPassword _ [] = Left "No such user in DB"
    checkPassword p (u : _)
      | validatePassword p $ pass u = Right u
      | otherwise = Left "No such user in DB"

-- Returns the user ID.
authorID :: W.Request -> IO Query
authorID req = do
  checkAuth' <- checkAuth (W.requestHeaders req)
  pure $ case checkAuth' of
    Right u -> fromString $ show $ user_id u
    _ -> "Null"

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> IO Bool
isAdmin req = do
  checkAuth' <- checkAuth (W.requestHeaders req)
  pure $ case checkAuth' of
    Right u -> is_admin u
    _ -> False

-- Checks the user's ability to create news.
isAuthor :: W.Request -> IO Bool
isAuthor req = do
  checkAuth' <- checkAuth (W.requestHeaders req)
  pure $ case checkAuth' of
    Right u -> is_author u
    _ -> False
