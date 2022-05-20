{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Category where


import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import Data.Monoid               ((<>))
import Data.List as LT           (find, filter) 
import qualified Data.Text as T
import qualified Data.ByteString.Char8  as BC     
import GHC.Generics 
import System.IO.Unsafe          (unsafePerformIO)
--import Auth
--import User



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

--'.../category?aaa>bbb'   aaa - parent category's name,  bbb - category's name 
createCategory :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createCategory False _ = "404"
createCategory _ [] = "404"
createCategory True ls 
  | categorys == [] = "404" 
  | otherwise = Query $
      "INSERT INTO category (name_category, parent_category) \
      \ VALUES ('" <> name_category <> "', '" <> parent_category <> "');"
  where
    (x:xs) = map (BC.split '>' . fst) ls 
    categorys = filter (/= "") x       
    [parent_category,name_category] = 
       if length categorys == 1 then "Null" : categorys else categorys
        

-- '.../category?change_name=aaa>bbb'    aaa - old category's name,  bbb - new category's name
-- '.../category?change_parent=aaa>bbb'  aaa - category's name,  bbb - new parent category's name
editCategory :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
editCategory False _ = "404"
editCategory _ [] = "404"
editCategory True ls 
  | fls == [] = "404" 
  | otherwise = Query $ checkQuery $ map buildQuery fls''               
  where
    fls = filter ((/="???") . snd) $ map (fmap fromMaybe) ls 
    fls' = map (fmap (BC.split '>')) fls 
    categorys = map ((filter (/= "")) <$>) fls'       
    fls''@((method,[name,new_name]):xs) = map (\x -> if length (snd x) < 2 
                                       then ("404", ["",""]) else x) categorys
    fromMaybe (Just e) = e
    fromMaybe Nothing  = "???"
    checkQuery lq = if elem "404" lq then "404" else mconcat lq
    buildQuery (method,[name,new_name]) = 
      case method of
        "change_name" ->     
          "UPDATE category \
          \ SET   parent_category = '" <> new_name <> "' \
          \ WHERE parent_category = '" <> name <> "'; \
          \ UPDATE category \
          \ SET   name_category = '" <> new_name <> "' \
          \ WHERE name_category = '" <> name <> "'; "    
        "change_parent" -> 
          "UPDATE category \
          \ SET parent_category = '" <> new_name <> "' \
          \ WHERE name_category = '" <> name <> "'; " 
        _ -> "404"








