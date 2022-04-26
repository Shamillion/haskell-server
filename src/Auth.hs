{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Network.Wai
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types.Header
import qualified Data.ByteString.Base64.Lazy as BB
import Data.String                   (fromString)




checkAuth :: RequestHeaders -> Either String L.ByteString
checkAuth ls = 
  if fls == [] then Left "No auth"
    else BB.decode . fromString . last . words . filter (/='"') . show $ str             
  where
    fls = filter  ((=="Authorization") . fst) ls
    [(hdr, str)] = fls
    
