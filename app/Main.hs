{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Control.Concurrent.MVar
--import           Control.Monad                   (join)
import           Data.Monoid               ((<>))
import           qualified Data.Text as T 
import           Data.Text.Encoding        (encodeUtf8)
--import qualified Data.Text.Lazy.Encoding as LE   (encodeUtf8)
import           GHC.Generics
import           Network.HTTP.Simple        
import           Network.HTTP.Types        (status200, status403, status404, 
                                                              queryToQueryText)
import           Network.Wai                as W
import           Network.Wai.Handler.Warp        (run)
import           Database.PostgreSQL.Simple as DB    
import           User
import           Category
import           News
import           Auth
import           Photo








setQueryAndRespond :: W.Request -> (DB.Query, ([[T.Text]] -> LC.ByteString))
setQueryAndRespond req = case (reqMtd, entity) of
  ("GET",  "news") -> (getNews authId method, encode . (map parseNews))
  ("POST", "news") -> (createNews athr authId arr, \[[x]] -> encode x)
  ("GET",  "user") -> (getUser "", encode . (map parseUser))
  ("POST", "user") -> (createUser adm arr, \[[x]] -> encode x)
  ("GET",  "category") -> (getCategory, encode . (map parseCategory))
  ("POST", "category") -> (createCategory adm arr, \[[x]] -> encode x)
  ("PUT",  "category") -> (editCategory adm arr, \[[x]] -> encode x)
  ("GET",  "photo")  -> (getPhoto arr, decodeImage)
  _                -> ("404", \x -> "404")
  where
    reqMtd = requestMethod req
    [entity] = pathInfo req 
    authId = authForGetNews req
    arr = queryString  req
    method = setMethodNews . queryToQueryText $ arr
    adm = isAdmin req
    athr = isAuthor req 

       
app :: Application
app req respond = do
  let (qry,resp) = setQueryAndRespond req
  print $ requestMethod req  
  print $ requestHeaders req 
  print $ checkAuth $ requestHeaders req  
  print $ pathInfo req 
  print $ queryString req 
  print qry
  case qry of
    "404" -> responds status403 "text/plain" "404 Not Found"
    _     -> do                    
      conn <- connectPostgreSQL "host='localhost' port=5432 \ 
               \ dbname='haskellserverlite' user='haskell' password='haskell'"
      val <- case requestMethod req of
        "GET" -> query_ conn qry :: IO [[T.Text]]         
        _     -> (\x -> [[T.pack $ show x]]) <$> execute_ conn qry 
      print val 
      let hdr = if pathInfo req == ["photo"] 
                  then  getHdr val'
                  else  "text/plain" 
          [[val']] = val                                
      responds status200 hdr $ resp val     
  where
    responds sts hdr = respond . responseLBS sts [("Content-Type", hdr)]  
    getHdr = encodeUtf8 . T.drop 1 . T.takeWhile (/=';') . T.dropWhile (/=':')  
    


    
                                                                     

main = do
  --  visitorCount <- newMVar 0
  --  run 4200 $ application visitorCount 
  run 4200 app
 -- print defaultConnectInfo
