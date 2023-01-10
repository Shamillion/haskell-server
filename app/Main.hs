{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Aeson
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
--import           Control.Concurrent.MVar
--import           Control.Monad                   (join)
--import           Data.Monoid               ((<>))
import           qualified Data.Text as T 
import           Data.Text.Encoding        (encodeUtf8)
--import qualified Data.Text.Lazy.Encoding as LE   (encodeUtf8)
--import           GHC.Generics
--import           Network.HTTP.Simple        
import           Network.HTTP.Types        (status200, status406, status404, 
                                                              queryToQueryText)
import           Network.Wai                as W
import           Network.Wai.Handler.Warp        (run)
import           Database.PostgreSQL.Simple as DB    
import           User
import           Category
import           News
import           Auth
import           Photo
import           Config
import           MigrationsDB (checkDB)
import           Lib          (drawOut)






setQueryAndRespond :: W.Request -> (DB.Query, ([[T.Text]] -> LC.ByteString))
setQueryAndRespond req = case (reqMtd, entity) of
  ("GET",  "news") -> (getNews authId method, encode . (map parseNews))
  ("POST", "news") -> (createNews athr authId arr, encodeWith)
  ("PUT",  "news") -> (editNews authId arr, encodeWith)
  ("GET",  "user") -> (getUser limitOffset, encode . (map parseUser))
  ("POST", "user") -> (createUser adm arr, encodeWith)
  ("GET",  "category") -> (getCategory limitOffset, encode . (map parseCategory))
  ("POST", "category") -> (createCategory adm arr, encodeWith)
  ("PUT",  "category") -> (editCategory adm arr, encodeWith)
  ("GET",  "photo")  -> (getPhoto arr, decodeImage)  
  _                -> ("404", \_ -> "404")
  where
    reqMtd = requestMethod req
    [entity] = pathInfo req 
    authId = authorID req
    arr = queryString  req
    method = setMethodNews limitElem . queryToQueryText $ arr
    limitOffset = setLimitAndOffset . queryToQueryText $ arr
    adm = isAdmin req
    athr = isAuthor req 
    encodeWith = encode . drawOut

       
app :: Application
app req respond = do
  writingLine INFO "Received a request."
  let (qry,resp) = setQueryAndRespond req
  writingLineDebug $ requestMethod req  
  writingLineDebug $ requestHeaders req 
  writingLineDebug $ checkAuth $ requestHeaders req  
  writingLineDebug $ pathInfo req 
  writingLineDebug $ queryString req 
  writingLineDebug qry
  case qry of
    "404"   -> responds status404 "text/plain" "404 Not Found.\n"
    "406uu" -> responds status406 "text/plain" "This login is already in use.\n"
    "406cu" -> responds status406 "text/plain" "This category already exists.\n"
    "406cn" -> responds status406 "text/plain" "There is no such category.\n"
    "406cp" -> responds status406 "text/plain" "There is no such parent \
                                                                   \category.\n"
    "406ce" -> responds status406 "text/plain" "A category cannot be a parent \
                                                                  \to itself.\n"
                                                                      
    _     -> do                    
      conn <- connectDB
      val <- case requestMethod req of
        "GET" -> query_ conn qry :: IO [[T.Text]]         
        _     -> (\x -> [[T.pack $ show x]]) <$> execute_ conn qry 
      writingLineDebug val 
      close conn
      let hdr = if pathInfo req == ["photo"] 
                  then  getHdr val'
                  else  "text/plain" 
          val' = drawOut val 
      writingLine INFO "Sent a response to the request."                                   
      responds status200 hdr $ resp val     
  where
    responds sts hdr = respond . responseLBS sts [("Content-Type", hdr)]  
    getHdr = encodeUtf8 . T.drop 1 . T.takeWhile (/=';') . T.dropWhile (/=':')  
    




    
                                                                     
main :: IO ()
main = do
  x <- checkDB 1
  writingLine DEBUG $ "checkDB was runing " <> show x <> " times."
  if x > 2 
    then print ("Error Database! Server can not be started!" :: String)
    else do
      writingLine INFO "Server is started."
      run port app
 
