{-# LANGUAGE OverloadedStrings #-}


module Photo where

import           Data.Char                  (isDigit)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import qualified Data.ByteString.Char8 as BC 
import           Data.Monoid                     ((<>))






getPhoto :: [(BC.ByteString, Maybe BC.ByteString)] -> Query
getPhoto [] = "404"
getPhoto (x:xs) = Query $
  case x of
  (_, Nothing) -> "404"
  (_, Just "") -> "404"
  (_, Just n)  -> 
    if BC.all isDigit n
      then "SELECT image FROM photo WHERE photo_id = " <> n <> ";"
      else "404"
