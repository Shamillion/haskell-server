{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Crypto.KDF.BCrypt (validatePassword)
import           Network.Wai
--import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types.Header
import qualified Data.ByteString.Base64 as BB
import Data.ByteString.Char8   as BC       (split, pack)
--import Data.ByteString.UTF8 (toString)
import Data.String          (fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import qualified Network.Wai            as W
import qualified Data.Text              as T
import System.IO.Unsafe          (unsafePerformIO)
import User
import Config




checkAuth :: RequestHeaders -> Either String [User]
checkAuth ls = 
  if fls == [] then Left "No Authorization"
    else 
      case decodeLogAndPass of
        Left x -> unsafePerformIO $ do 
                  writingLine ERROR x 
                  pure $ Left x
        Right [x,y] -> isEmptyList $ unsafePerformIO $ do
          conn <- connectDB       
          let qry = getUser $ Query $ "WHERE login = '" <> x <> "'"  
          writingLineDebug qry                                               
          userList <- query_ conn $ qry :: IO [[T.Text]] 
          writingLineDebug userList
          pure $ checkPassword y userList       
  where
    fls = filter  ((=="Authorization") . fst) ls
    [(hdr, str)] = fls
    decodeLogAndPass = (split ':') <$> (BB.decode . last . BC.split ' ' $ str)
    isEmptyList [] = Left "No such user in DB"      
    isEmptyList ul = Right ul
    checkPassword _ [] = []
    checkPassword p (u:_)
      | u == [] = []
      | otherwise = 
          if (validatePassword p $ (\(h:_) -> BC.pack $ T.unpack h) $ reverse u)
          then [parseUser u]
          else [] 

authorID :: W.Request -> Query
authorID req = 
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
  
  
  
  
