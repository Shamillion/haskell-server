{-# LANGUAGE OverloadedStrings #-}

module Config where

--import qualified Data.ByteString.Char8      as BC
import           Database.PostgreSQL.Simple 

connectDB :: IO Connection
connectDB = connectPostgreSQL "host='localhost' port=5432 \
               \ dbname='haskellserverlite' user='haskell' password='haskell'"
