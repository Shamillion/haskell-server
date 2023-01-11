{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Category where


import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
-- import Data.Monoid               ((<>))
import Data.List as LT           (find) 
import qualified Data.Text as T
import qualified Data.ByteString.Char8  as BC     
import GHC.Generics 
import System.IO.Unsafe          (unsafePerformIO)
--import Auth
--import User
import Config
import Lib                                           (head', fromMaybe, readNum)


getCategory :: Query -> Query
getCategory limitOffset = "SELECT (category_id :: TEXT), parent_category, \
              \ name_category FROM category " <> limitOffset <> ";"
              
getCategory' :: Query
getCategory' = "SELECT parent_category, name_category FROM category;"              

data Category = Category
  { category_id     :: Int 
  , parent_category :: T.Text
  , name_category   :: T.Text  
  } 
   deriving (Show, Generic, ToJSON)
   
errorCategory :: Category
errorCategory =  Category 0 "error" "error" 

parseCategory :: [T.Text] -> Category
parseCategory ls
  | length ls /= 3 = errorCategory
  | idCat == 0 = errorCategory
  | otherwise = Category idCat pc nc 
  where
    idCat = readNum ic
    (ic:pc:nc:_) = ls
    

getParentCategories :: T.Text -> [T.Text] 
getParentCategories cat = unsafePerformIO $ do
  conn <- connectDB
  ls <- query_ conn getCategory' :: IO [[T.Text]]
  close conn
  writingLineDebug ls
  let buildingList pc = do
        let val = LT.find (\(_:y:_) -> y == head' pc) ls 
        case val of
          Just el -> buildingList (head' el : pc)
          _       -> pc 
  pure $ filter (/="Null") $ buildingList [cat]

--'.../category?aaa>bbb'   aaa - parent category's name,  bbb - category's name 
createCategoryWith :: (BC.ByteString -> Bool) -> Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createCategoryWith _ False _ = "404"
createCategoryWith _ _ [] = "404"
createCategoryWith checkUniq True ls 
  | null categorys = "404" 
  | not (checkUniq nameCategory) = "406cu"
  | checkUniq parentCategory && parentCategory /= "Null" = "406cp"
  | otherwise = Query $
      "INSERT INTO category (name_category, parent_category) \
      \ VALUES ('" <> nameCategory <> "', '" <> parentCategory <> "');"
  where
    (x:_) = map (BC.split '>' . fst) ls 
    categorys = filter (/= "") x       
    (parentCategory : nameCategory : _) = 
       if length categorys == 1 then "Null" : categorys else categorys

createCategory :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createCategory = createCategoryWith checkUniqCategory       

-- '.../category?change_name=aaa>bbb'    aaa - old category's name,  bbb - new category's name
-- '.../category?change_parent=aaa>bbb'  aaa - category's name,  bbb - new parent category's name
editCategoryWith :: (BC.ByteString -> Bool) -> Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
editCategoryWith _ False _ = "404"
editCategoryWith _ _ [] = "404"
editCategoryWith checkUniq True ls 
  | null fls = "404"
  | "" `elem` [name,new_name] = "404"
  | checkUniq name = "406cn"
  | method == "change_name" && not (checkUniq new_name) = "406cu"
  | method == "change_parent" && 
         checkUniq new_name && new_name /= "Null" = "406cp"
  | method == "change_parent" && name == new_name = "406ce"
  | otherwise = Query $ checkQuery $ map buildQuery fls''               
  where
    fls = filter ((/="???") . snd) $ map (fmap fromMaybe) $ take 1 ls 
    fls' = map (fmap (BC.split '>')) fls 
    categorys = map (filter (/= "") <$>) fls'       
    fls''@((method,[name,new_name]):_) = map (\x -> if length (snd x) /= 2 
                                       then ("404", ["",""]) else x) categorys
    checkQuery lq = if "404" `elem` lq then "404" else mconcat lq
    buildQuery (meth,[nm,new_nm]) = 
      case meth of
        "change_name" ->     
          "UPDATE category \
          \ SET   parent_category = '" <> new_nm <> "' \
          \ WHERE parent_category = '" <> nm <> "'; \
          \ UPDATE category \
          \ SET   name_category = '" <> new_nm <> "' \
          \ WHERE name_category = '" <> nm <> "'; "    
        "change_parent" -> 
          "UPDATE category \
          \ SET parent_category = '" <> new_nm <> "' \
          \ WHERE name_category = '" <> nm <> "'; " 
        _ -> "404"
    buildQuery (_,_) = "404"   

editCategory :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
editCategory = editCategoryWith checkUniqCategory

checkUniqCategory :: BC.ByteString -> Bool
checkUniqCategory str = unsafePerformIO $ do
  conn <- connectDB
  ls <- query_ conn $ Query $ "SELECT name_category FROM category WHERE \
                     \ name_category = '" <> str <> "';" :: IO [[BC.ByteString]]
  close conn
  writingLineDebug ls
  pure $ null ls




