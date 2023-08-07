{-# LANGUAGE LambdaCase #-}

module Main where

import Auth (authorID, checkAuth, isAdmin, isAuthor)
import Category (categoryHandler, createCategory, editCategory, getCategory, parseCategory)
import Config (Priority (..), port, writingLine, writingLineDebug)
import Control.Exception (catch)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import Error (CategoryError (..), Error (..))
import Lib (drawOut)
import MigrationsDB (checkDB)
import Network.HTTP.Types (queryToQueryText, status200, status404, status406)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import News (createNews, createNewsHandler, editNews, getNewsHandler, newsHandler, setLimitAndOffset)
import Photo (decodeImage, getPhoto)
import User (blockAdminRights, createUser, getUserAsText, parseUser)

handler :: W.Request -> IO LC.ByteString
handler req = do
  case (reqMethod, entity) of
    ("GET", "news") -> getNewsHandler req
    ("POST", "news") -> createNewsHandler req
    --     ("PUT", "news") -> editNews req
    --     ("GET", "user") -> getUserAsText req
    --     ("GET", "category") -> getCategory req
    --     ("POST", "category") -> createCategory req
    --     ("PUT", "category") -> editCategory req
    --     ("GET", "photo") -> getPhoto req
    _ -> pure "Error"
  where
    reqMethod = W.requestMethod req
    [entity] = W.pathInfo req

-- Defining the type of request and creating a response.
setQueryAndRespond :: W.Request -> IO (Either Error DB.Query, [[T.Text]] -> IO (Either Error LC.ByteString))
setQueryAndRespond req = do
  case (reqMethod, entity) of
    -- ("GET", "news") ->
    --   (,)
    --     <$> (getNews <$> authId <*> method)
    --     <*> pure (fmap (fmap encode . sequence) . mapM parseNews)
    ("POST", "news") ->
      (,)
        <$> createNews athr authId arr
        <*> pure encodeWith
    ("PUT", "news") ->
      (,)
        <$> editNews authId arr
        <*> pure encodeWith
    ("GET", "user") ->
      (,)
        <$> (pure . getUserAsText <$> limitOffset)
        <*> pure (pure . fmap encode . mapM parseUser)
    ("POST", "user") ->
      (,)
        <$> createUser adm arr
        <*> pure encodeWith
    ("PUT", "user") ->
      (,)
        <$> (blockAdminRights <$> adm)
        <*> pure encodeWith
    ("GET", "category") ->
      (,)
        <$> (getCategory <$> limitOffset)
        <*> pure (pure . fmap encode . mapM parseCategory)
    ("POST", "category") ->
      (,)
        <$> createCategory categoryHandler adm arr
        <*> pure encodeWith
    ("PUT", "category") ->
      (,)
        <$> editCategory categoryHandler adm arr
        <*> pure encodeWith
    ("GET", "photo") -> pure (getPhoto arr, pure . decodeImage)
    _ -> pure (Left CommonError, const (pure $ Left CommonError))
  where
    reqMethod = W.requestMethod req
    [entity] = W.pathInfo req
    authId = authorID req
    arr = W.queryString req
    --  method = setMethodNews newsHandler . queryToQueryText $ arr
    limitOffset = setLimitAndOffset newsHandler . queryToQueryText $ arr
    adm = isAdmin req
    athr = isAuthor req
    encodeWith =
      pure
        . pure
        . (<> " position(s) done.")
        . LC.fromStrict
        . encodeUtf8
        . drawOut

app :: W.Application
app req respond = do
  writingLine INFO "Received a request."
  -- (eitherQry, resp) <- setQueryAndRespond req
  writingLineDebug $ W.requestMethod req
  writingLineDebug $ W.requestHeaders req
  writingLineDebug =<< checkAuth (W.requestHeaders req)
  writingLineDebug $ W.pathInfo req
  writingLineDebug $ W.queryString req
  ans <-
    catch
      (handler req)
      ( \case
          LoginOccupied ->
            respondsSts406 "This login is already in use.\n" >> pure ""
          CategoryError CategoryExists ->
            respondsSts406 "This category already exists.\n" >> pure ""
          CategoryError NoCategory ->
            respondsSts406 "There is no such category.\n" >> pure ""
          CategoryError NoParentCategory ->
            respondsSts406 "There is no such parent category.\n" >> pure ""
          CategoryError CategoryParentItself ->
            respondsSts406 "A category cannot be a parent to itself.\n" >> pure ""
          _ -> responds status404 "text/plain" "404 Not Found.\n" >> pure ""
      )
  -- Right qry -> do
  --   conn <- connectDB
  --   dataFromDB <- case W.requestMethod req of
  --     "GET" -> DB.query_ conn qry :: IO [[T.Text]]
  --     _ -> (\x -> [[T.pack $ show x]]) <$> DB.execute_ conn qry
  --   writingLineDebug dataFromDB
  --   DB.close conn
  let header = "text/plain"
  --   if W.pathInfo req == ["photo"]
  --     then getHeader headerForPhoto
  --     else "text/plain"
  -- headerForPhoto = drawOut dataFromDB
  --   writingLine INFO "Sent a response to the request."
  --   eitherAns <- resp dataFromDB
  --   writingLineDebug eitherAns
  --   case eitherAns of
  --     Left err -> do
  --       writingLine ERROR $ show err
  --       responds status404 "text/plain" "404 Not Found.\n"
  responds status200 header ans
  where
    responds status header = respond . W.responseLBS status [("Content-Type", header)]
    respondsSts406 = responds status406 "text/plain"

--   getHeader = encodeUtf8 . T.drop 1 . T.takeWhile (/= ';') . T.dropWhile (/= ':')

main :: IO ()
main = do
  num <- checkDB 1
  writingLine DEBUG $ "checkDB was runing " <> show num <> " times."
  if num > 2
    then putStrLn "Error Database! Server can not be started!"
    else do
      port' <- port
      mapM_ (\func -> func "Server is started.") [putStrLn, writingLine INFO]
      run port' app
