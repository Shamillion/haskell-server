module Main where

import Auth (authorID, checkAuth, isAdmin, isAuthor)
import Category (categoryHandler, createCategory, editCategory, getCategory, parseCategory)
import Config (Priority (..), connectDB, port, writingLine, writingLineDebug)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import Error (Error (..))
import Lib (drawOut)
import MigrationsDB (checkDB)
import Network.HTTP.Types (queryToQueryText, status200, status404, status406)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import News (createNews, editNews, getNews, newsHandler, parseNews, setLimitAndOffset, setMethodNews)
import Photo (decodeImage, getPhoto)
import User (blockAdminRights, createUser, getUser, parseUserWithoutPass)

-- Defining the type of request and creating a response.
setQueryAndRespond :: W.Request -> IO (Either Error DB.Query, [[T.Text]] -> IO LC.ByteString)
setQueryAndRespond req = do
  case (reqMtd, entity) of
    ("GET", "news") ->
      (,)
        <$> (getNews <$> authId <*> method)
        <*> pure ((encode <$>) . mapM parseNews)
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
        <$> (pure . getUser <$> limitOffset)
        <*> pure (pure . encode . map parseUserWithoutPass)
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
        <*> pure (pure . encode . map parseCategory)
    ("POST", "category") ->
      (,)
        <$> createCategory categoryHandler adm arr
        <*> pure encodeWith
    ("PUT", "category") ->
      (,)
        <$> editCategory categoryHandler adm arr
        <*> pure encodeWith
    ("GET", "photo") -> pure (getPhoto arr, pure . fromEither . decodeImage)
    _ -> pure (Left CommonError, const (pure "Error"))
  where
    reqMtd = W.requestMethod req
    [entity] = W.pathInfo req
    authId = authorID req
    arr = W.queryString req
    method = setMethodNews newsHandler . queryToQueryText $ arr
    limitOffset = setLimitAndOffset newsHandler . queryToQueryText $ arr
    adm = isAdmin req
    athr = isAuthor req
    encodeWith =
      pure . (<> " position(s) done.") . LC.fromStrict . encodeUtf8 . drawOut
    fromEither x = case x of
      Right str -> str
      _ -> mempty

app :: W.Application
app req respond = do
  writingLine INFO "Received a request."
  (eitherQry, resp) <- setQueryAndRespond req
  writingLineDebug $ W.requestMethod req
  writingLineDebug $ W.requestHeaders req
  writingLineDebug =<< checkAuth (W.requestHeaders req)
  writingLineDebug $ W.pathInfo req
  writingLineDebug $ W.queryString req
  writingLineDebug eitherQry
  case eitherQry of
    Left CommonError -> responds status404 "text/plain" "404 Not Found.\n"
    Left LoginOccupied -> respondsSts406 "This login is already in use.\n"
    Left CategoryExists -> respondsSts406 "This category already exists.\n"
    Left NoCategory -> respondsSts406 "There is no such category.\n"
    Left NoParentCategory -> respondsSts406 "There is no such parent category.\n"
    Left CategoryParentItself -> respondsSts406 "A category cannot be a parent to itself.\n"
    Right qry -> do
      conn <- connectDB
      val <- case W.requestMethod req of
        "GET" -> DB.query_ conn qry :: IO [[T.Text]]
        _ -> (\x -> [[T.pack $ show x]]) <$> DB.execute_ conn qry
      writingLineDebug val
      DB.close conn
      let hdr =
            if W.pathInfo req == ["photo"]
              then getHdr val'
              else "text/plain"
          val' = drawOut val
      writingLine INFO "Sent a response to the request."
      ans <- resp val
      responds status200 hdr ans
  where
    responds sts hdr = respond . W.responseLBS sts [("Content-Type", hdr)]
    respondsSts406 = responds status406 "text/plain"
    getHdr = encodeUtf8 . T.drop 1 . T.takeWhile (/= ';') . T.dropWhile (/= ':')

main :: IO ()
main = do
  x <- checkDB 1
  writingLine DEBUG $ "checkDB was runing " <> show x <> " times."
  if x > 2
    then print ("Error Database! Server can not be started!" :: String)
    else do
      port' <- port
      mapM_ (\f -> f "Server is started.") [print, writingLine INFO]
      run port' app
