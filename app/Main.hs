{-# LANGUAGE LambdaCase #-}

module Main where

import Auth (isAdmin)
import Category (getCategoryHandler, mkCreateCategoryQuery, mkEditCategoryQuery)
import Config (Priority (..), port, writingLine, writingLineDebug)
import Control.Exception (catch, throwIO)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy.Char8 as LC
import Error (CategoryError (..), Error (..))
import Lib (createAndEditObjectsHandler)
import MigrationsDB (checkDB)
import Network.HTTP.Types (status200, status404, status406)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import News (getNewsHandler, mkCreateNewsQuery, mkEditNewsQuery)
import Photo (getPhotoHandler)
import User (getUserHandler, mkCreateUserQuery, mkEditUserQuery)

handler :: W.Request -> IO LC.ByteString
handler req = do
  case (reqMethod, entity) of
    ("GET", "news") -> getNewsHandler req
    ("POST", "news") -> createAndEditObjectsHandler mkCreateNewsQuery req
    ("PUT", "news") -> createAndEditObjectsHandler mkEditNewsQuery req
    ("GET", "user") -> getUserHandler req
    ("POST", "user") -> createAndEditObjectsHandler (mkCreateUserQuery adm) req
    ("PUT", "user") -> createAndEditObjectsHandler (mkEditUserQuery adm) req
    ("GET", "category") -> getCategoryHandler req
    ("POST", "category") -> createAndEditObjectsHandler (mkCreateCategoryQuery adm) req
    ("PUT", "category") -> createAndEditObjectsHandler (mkEditCategoryQuery adm) req
    ("GET", "photo") -> getPhotoHandler req
    _ -> throwIO CommonError
  where
    reqMethod = W.requestMethod req
    [entity] = W.pathInfo req
    adm = isAdmin req

app :: W.Application
app req respond = do
  writingLine INFO "Received a request."
  writingLineDebug $ W.requestMethod req
  writingLineDebug $ W.requestHeaders req
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
  let (header, answer) =
        if W.pathInfo req == ["photo"]
          then headerAndImage ans
          else ("text/plain", ans)
  responds status200 header answer
  where
    responds status header = respond . W.responseLBS status [("Content-Type", header)]
    respondsSts406 = responds status406 "text/plain"
    headerAndImage = BF.first LC.toStrict . fmap (LC.drop 1) . LC.span (/= ';')

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
