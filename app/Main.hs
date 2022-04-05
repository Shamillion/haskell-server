{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Control.Concurrent.MVar
--import           Control.Monad                   (join)
import           Data.Monoid                     ((<>))
import           qualified Data.Text as T hiding (last)
import           GHC.Generics
import           Network.HTTP.Simple        
import           Network.HTTP.Types              (status200, queryToQueryText)
import           Network.Wai                as W
import           Network.Wai.Handler.Warp        (run)
import           Database.PostgreSQL.Simple as DB    
import           User
import           Db
import           News
--import           Data.ByteString.Base64
   


setQueryAndRespond :: W.Request -> (DB.Query, ([[T.Text]] -> LC.ByteString))
setQueryAndRespond req = case entity of
  "news" -> (getNews (setMethodNews method), encode . (map parseNews))
  _      -> (getUser, encode . (map parseUser))
  where
    [entity] = pathInfo req 
    method   = queryToQueryText $ queryString req


       
app :: Application
app req respond = do
    let (qry,resp) = setQueryAndRespond req
    conn <- connectPostgreSQL "host='localhost' port=5432 \ 
               \ dbname='haskellserverlite' user='haskell' password='haskell'"
    val <- query_ conn qry :: IO [[T.Text]]     
    print $ pathInfo req 
    print $ queryToQueryText $ queryString req 
    print $ queryString req    
    print val  
   -- print $ map parseNews val
    respond $ responseLBS status200 [] $ resp val 

main = do
  --  visitorCount <- newMVar 0
  --  run 4200 $ application visitorCount 
   run 4200 app
 -- print defaultConnectInfo
