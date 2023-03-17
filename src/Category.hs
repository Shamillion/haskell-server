{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Category where

import Config (connectDB, writingLineDebug)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Char8 as BC
import Data.List as LT (find)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import GHC.Generics (Generic)
import Lib (fromMaybe, head', readNum)

-- Creating a database query to get a list of catygories.
getCategory :: Query -> Query
getCategory limitOffset =
  "SELECT (category_id :: TEXT), parent_category, \
  \ name_category FROM category "
    <> limitOffset
    <> ";"

-- For getParentCategories.
getCategory' :: Query
getCategory' = "SELECT parent_category, name_category FROM category;"

data Category = Category
  { category_id :: Int,
    parent_category :: T.Text,
    name_category :: T.Text
  }
  deriving (Show, Generic, ToJSON)

errorCategory :: Category
errorCategory = Category 0 "error" "error"

parseCategory :: [T.Text] -> Category
parseCategory ls
  | length ls /= 3 = errorCategory
  | idCat == 0 = errorCategory
  | otherwise = Category idCat pc nc
  where
    idCat = readNum ic
    (ic : pc : nc : _) = ls

data CategoryHandle m = CategoryHandle 
  {checkUniqCategoryH :: BC.ByteString -> m Bool}
    
categoryHandler :: CategoryHandle IO  
categoryHandler = CategoryHandle {checkUniqCategoryH = checkUniqCategory}    

getParentCategories :: T.Text -> IO [T.Text]
getParentCategories cat = do
  conn <- connectDB
  ls <- query_ conn getCategory' :: IO [[T.Text]]
  close conn
  writingLineDebug ls
  let buildingList pc = do
        let val = LT.find (\(_ : y : _) -> y == head' pc) ls
        case val of
          Just el -> buildingList (head' el : pc)
          _ -> pc
  pure $ filter (/= "Null") $ buildingList [cat]

-- Request example:
--  '.../category?aaa>bbb'
--      aaa - parent category's name,
--      bbb - category's name.
createCategory :: Monad m =>
  CategoryHandle m -> m Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> m Query
createCategory CategoryHandle {..} isAdm ls = do
  isAdm' <- isAdm
  if not isAdm' || null ls || null categorys 
    then pure "404"
    else do
      uniqName <- checkUniqCategoryH nameCategory
      uniqParent <- checkUniqCategoryH parentCategory
      pure $ checkAndResponse uniqName uniqParent       
  where
    (x : _) = map (BC.split '>' . fst) ls
    categorys = filter (/= "") x
    (parentCategory : nameCategory : _) =
      if length categorys == 1 then "Null" : categorys else categorys      
    checkAndResponse uNm uPrnt
      | not uNm = "406cu"     
      | uPrnt && parentCategory /= "Null" = "406cp" 
      | otherwise = Query $
                "INSERT INTO category (name_category, parent_category) \
                \ VALUES ('"
                  <> nameCategory
                  <> "', '"
                  <> parentCategory
                  <> "');"

-- Request examples:
--   Changing the category name: '.../category?change_name=aaa>bbb'
--      aaa - old category's name,
--      bbb - new category's name.
--   Changing the parent category: '.../category?change_parent=aaa>bbb'
--      aaa - category's name,
--      bbb - new parent category's name.
editCategory :: Monad m =>
  CategoryHandle m -> m Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> m Query
editCategory CategoryHandle {..} isAdm ls = do
  isAdm' <- isAdm
  if not isAdm' || null ls || null fls || "" `elem` [name, new_name]
    then pure "404"
    else do      
      uniqName <- checkUniqCategoryH name
      if uniqName
        then pure "406cn"
        else do
          uniqNew_name <- checkUniqCategoryH new_name
          pure $ checkAndResponse uniqNew_name  
  where
    fls = filter ((/= "???") . snd) $ map (fmap fromMaybe) $ take 1 ls
    fls' = map (fmap (BC.split '>')) fls
    categorys = map (filter (/= "") <$>) fls'
    fls''@((method, [name, new_name]) : _) =
      map
        ( \x ->
            if length (snd x) /= 2
              then ("404", ["", ""])
              else x
        )
        categorys    
    checkAndResponse w
      | method == "change_parent" && name == new_name = "406ce"
      | method == "change_name" && not w = "406cu"
      | method == "change_parent" && w && new_name /= "Null" = "406cp" 
      | otherwise = Query $ checkQuery $ map buildQuery fls''       
    checkQuery lq = if "404" `elem` lq then "404" else mconcat lq
    buildQuery (meth, [nm, new_nm]) =
      case meth of
        "change_name" ->
          "UPDATE category \
          \ SET   parent_category = '"
            <> new_nm
            <> "' \
               \ WHERE parent_category = '"
            <> nm
            <> "'; \
               \ UPDATE category \
               \ SET   name_category = '"
            <> new_nm
            <> "' \
               \ WHERE name_category = '"
            <> nm
            <> "'; "
        "change_parent" ->
          "UPDATE category \
          \ SET parent_category = '"
            <> new_nm
            <> "' \
               \ WHERE name_category = '"
            <> nm
            <> "'; "
        _ -> "404"
    buildQuery (_, _) = "404"

-- Checking the uniqueness of the category name in the database.
checkUniqCategory :: BC.ByteString -> IO Bool
checkUniqCategory str = do
  conn <- connectDB
  ls <-
    query_ conn $
      Query $
        "SELECT name_category FROM category WHERE \
        \ name_category = '"
          <> str
          <> "';" ::
      IO [[BC.ByteString]]
  close conn
  writingLineDebug ls
  pure $ null ls
