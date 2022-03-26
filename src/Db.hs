{-# LANGUAGE OverloadedStrings #-}
module Db where

import Database.PostgreSQL.Simple
import Data.Monoid               ((<>))


getCategory :: Query
getCategory = "SELECT parent_category, name_category \
               \ FROM category;"



   

