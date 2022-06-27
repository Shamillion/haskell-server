{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Network.Wai
--import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types.Header
import qualified Data.ByteString.Base64 as BB
import Data.ByteString.Char8          (split)
--import Data.ByteString.UTF8 (toString)
import Data.String          (fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import qualified Network.Wai            as W
import qualified Data.Text              as T
import System.IO.Unsafe          (unsafePerformIO)
import User





checkAuth :: RequestHeaders -> Either String [User]
checkAuth ls = 
  if fls == [] then Left "No Authorization"
    else 
      case decodeLogAndPass of
        Left x -> Left x
        Right [x,y] -> isEmptyList $ unsafePerformIO $ do
          conn <- connectPostgreSQL "host='localhost' port=5432 \ 
                 \ dbname='haskellserverlite' user='haskell' password='haskell'"       
          let qry = getUser $ Query $ 
                         "WHERE login = '" <> x <> "' AND pass = '" <> y <> "'"  
          print qry                                               
          userList <- query_ conn $ qry :: IO [[T.Text]] 
          print userList
          pure $ map parseUser userList
  where
    fls = filter  ((=="Authorization") . fst) ls
    [(hdr, str)] = fls
    decodeLogAndPass = (split ':') <$> (BB.decode . last . split ' ' $ str)
    isEmptyList [] = Left "No such user in DB"      
    isEmptyList ul = Right ul

authForGetNews :: W.Request -> Query
authForGetNews req = 
  case checkAuth (requestHeaders req) of     
    Right [u] -> fromString $ show $ user_id u    
    _         -> Query $ "Null" 
  
isAdmin :: W.Request -> Bool
isAdmin req = 
  case checkAuth (requestHeaders req) of     
    Right [u] -> is_admin u    
    _         -> False   
  
isAuthor :: W.Request -> Bool
isAuthor req = 
  case checkAuth (requestHeaders req) of     
    Right [u] -> is_author u    
    _         -> False  
  
  
  
  
