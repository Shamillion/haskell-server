{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Category where


import Data.Aeson
import Database.PostgreSQL.Simple
import Data.Monoid               ((<>))
import Data.List as LT           (find, filter) 
import qualified Data.Text as T
import GHC.Generics 
import System.IO.Unsafe          (unsafePerformIO)


getCategory :: Query
getCategory = "SELECT parent_category, name_category FROM category;"

data Category = Category
  { parent_category :: T.Text
  , name_category   :: T.Text  
  } 
   deriving (Show, Generic, ToJSON)
   
errorCategory :: Category
errorCategory =  Category "error" "error" 

parseCategory :: [T.Text] -> Category
parseCategory ls@(x:y:ys)
  | length ls /= 2 = errorCategory
  | otherwise = Category x y  
    

getParentCategories :: T.Text -> [T.Text] 
getParentCategories cat = unsafePerformIO $ do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='haskellserverlite' \
                             \ user='haskell' password='haskell'"
  ls <- query_ conn $ getCategory :: IO [[T.Text]]
  let buildingList pc = do
        let val = LT.find (\(x:y:xy) -> y == head pc) ls 
        case val of
          Just el -> buildingList (head el : pc)
          _       -> pc 
  pure $ filter (/="Null") $ buildingList [cat]
