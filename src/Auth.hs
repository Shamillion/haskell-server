{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Config (Priority (ERROR), connectDB, writingLine, writingLineDebug)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString.Char8 as BC
import Data.String (fromString)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Lib (last')
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Network.Wai as W
import System.IO.Unsafe (unsafePerformIO)
import User (User (..), getUser, parseUser)

-- Returns the user with the username and password from the request.
checkAuth :: RequestHeaders -> Either String [User]
checkAuth ls =
  if null fls
    then Left "No Authorization"
    else case decodeLogAndPass of
      Left x -> unsafePerformIO $ do
        writingLine ERROR x
        pure $ Left x
      Right [x, y] -> isEmptyList $
        unsafePerformIO $ do
          conn <- connectDB
          let qry = getUser $ Query $ "WHERE login = '" <> x <> "'"
          writingLineDebug qry
          userList <- query_ conn qry :: IO [[T.Text]]
          close conn
          writingLineDebug userList
          pure $ checkPassword y userList
      _ -> Left "No Authorization"
  where
    fls = filter ((== "Authorization") . fst) ls
    [(_, str)] = fls
    decodeLogAndPass = BC.split ':' <$> (BB.decode . last' . BC.split ' ' $ str)
    isEmptyList [] = Left "No such user in DB"
    isEmptyList ul = Right ul
    checkPassword _ [] = []
    checkPassword p (u : _)
      | null u = []
      | otherwise =
        [parseUser u | validatePassword p $ BC.pack . T.unpack . last' $ u]

-- Returns the user ID.
authorID :: W.Request -> Query
authorID req =
  case checkAuth (W.requestHeaders req) of
    Right [u] -> fromString $ show $ user_id u
    _ -> Query "Null"

-- Checks the administrator rights of the user.
isAdmin :: W.Request -> Bool
isAdmin req =
  case checkAuth (W.requestHeaders req) of
    Right [u] -> is_admin u
    _ -> False

-- Checks the user's ability to create news.
isAuthor :: W.Request -> Bool
isAuthor req =
  case checkAuth (W.requestHeaders req) of
    Right [u] -> is_author u
    _ -> False
