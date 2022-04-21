{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Control.Concurrent.MVar
--import           Control.Monad                   (join)
import           Data.Monoid                     ((<>))
import           qualified Data.Text as T 
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
--import           Data.ByteString.Base64
   


setQueryAndRespond :: W.Request -> (DB.Query, ([[T.Text]] -> LC.ByteString))
setQueryAndRespond req = case (reqMtd, entity) of
  ("GET", "news")  -> (getNews (setMethodNews method), encode . (map parseNews))
  ("GET", "users") -> (getUser, encode . (map parseUser))
  ("GET", "category") -> (getCategory, encode . (map parseCategory))
  _                -> ("404", (\x -> "404"))
  where
    reqMtd = requestMethod req
    [entity] = pathInfo req 
    method   = queryToQueryText $ queryString req


       
app :: Application
app req respond = do
  let (qry,resp) = setQueryAndRespond req
  print $ requestMethod req
  print $ pathInfo req 
  print $ queryString req 
  print qry
  case qry of
    "403" -> responds status403 "403 Forbidden"
    "404" -> responds status403 "404 Not Found"
    _     -> do        
      conn <- connectPostgreSQL "host='localhost' port=5432 \ 
               \ dbname='haskellserverlite' user='haskell' password='haskell'"
      val <- query_ conn qry :: IO [[T.Text]] 
      print val  
     -- print $ map parseNews val
      responds status200 $ resp val 
  where
    responds sts = respond . responseLBS sts [("Content-Type", "text/plain")]                                                                   

main = do
  --  visitorCount <- newMVar 0
  --  run 4200 $ application visitorCount 
  run 4200 app
 -- print defaultConnectInfo
