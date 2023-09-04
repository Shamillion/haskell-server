module Main where

import Auth (isAdmin)
import Category (buildCreateCategoryQuery, buildEditCategoryQuery, getCategoryHandler)
import Config (Priority (..), port, writingLine, writingLineDebug)
import Control.Exception (catch, throwIO)
import qualified Data.ByteString.Lazy.Char8 as LC
import Error (CategoryError (..), Error (..))
import Lib (createAndEditObjectsHandler)
import MigrationsDB (checkDB)
import Network.HTTP.Types (status200, status404, status406)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import News (buildCreateNewsQuery, buildEditNewsQuery, getNewsHandler)
import Photo (getPhotoHandler, headerAndImage)
import User (buildCreateUserQuery, buildEditUserQuery, getUserHandler)

handler :: W.Request -> IO LC.ByteString
handler req = do
  admin <- isAdmin req
  let reqMethod = W.requestMethod req
      [entity] = W.pathInfo req
  case (reqMethod, entity) of
    ("GET", "news") -> getNewsHandler req
    ("POST", "news") -> createAndEditObjectsHandler buildCreateNewsQuery req
    ("PUT", "news") -> createAndEditObjectsHandler buildEditNewsQuery req
    ("GET", "user") -> getUserHandler req
    ("POST", "user") -> createAndEditObjectsHandler (buildCreateUserQuery admin) req
    ("PUT", "user") -> createAndEditObjectsHandler (buildEditUserQuery admin) req
    ("GET", "category") -> getCategoryHandler req
    ("POST", "category") -> createAndEditObjectsHandler (buildCreateCategoryQuery admin) req
    ("PUT", "category") -> createAndEditObjectsHandler (buildEditCategoryQuery admin) req
    ("GET", "photo") -> getPhotoHandler req
    _ -> throwIO CommonError

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
      ( \err ->
          "" <$ case err of
            LoginOccupied ->
              respondsSts406 "This login is already in use.\n"
            CategoryError CategoryExists ->
              respondsSts406 "This category already exists.\n"
            CategoryError NoCategory ->
              respondsSts406 "There is no such category.\n"
            CategoryError NoParentCategory ->
              respondsSts406 "There is no such parent category.\n"
            CategoryError CategoryParentItself ->
              respondsSts406 "A category cannot be a parent to itself.\n"
            _ -> responds status404 "text/plain" "404 Not Found.\n"
      )
  let (header, answer) =
        if W.pathInfo req == ["photo"]
          then headerAndImage ans
          else ("text/plain", ans)
  responds status200 header answer
  where
    responds status header = respond . W.responseLBS status [("Content-Type", header)]
    respondsSts406 = responds status406 "text/plain"

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
