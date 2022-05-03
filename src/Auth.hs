{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Network.Wai
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types.Header
import qualified Data.ByteString.Base64.Lazy as BB
import Database.PostgreSQL.Simple
import qualified Data.Text as T
import Data.String                   (fromString)
import System.IO.Unsafe          (unsafePerformIO)
import User




checkAuth :: RequestHeaders -> Either String [User]
checkAuth ls = 
  if fls == [] then Left "No auth"
    else 
      case decodeLogAndPass of
        Left x -> Left $ show x
        Right [x,y] -> pure $ unsafePerformIO $ do
          conn <- connectPostgreSQL "host='localhost' port=5432 \ 
                 \ dbname='haskellserverlite' user='haskell' password='haskell'"
          let [un, ps] = map (\v -> "'" ++ filter (/='"') (show v) ++ "'") [x,y] 
          let qry = getUser $ fromString $ "WHERE login = " <> 
                                                     un <> " AND pass = " <> ps    
          print qry                                               
          userList <- query_ conn $ qry :: IO [[T.Text]] 
          print userList
          pure $ map parseUser userList
  where
    fls = filter  ((=="Authorization") . fst) ls
    [(hdr, str)] = fls
    decodeLogAndPass = (L.split 58) <$>           
                       (BB.decode . last . L.split 32 . L.fromStrict $ str)
          
 
