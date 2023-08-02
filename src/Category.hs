{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Category where

import Config (connectDB, writingLineDebug)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Char8 as BC
import Data.List as LT (find)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error
  ( CategoryError (..),
    Error
      ( CategoryError,
        CommonError,
        ParseError
      ),
    ParseError (ParseCategoryError),
  )
import GHC.Generics (Generic)
import Lib (readNum)

-- Creating a database query to get a list of catygories.
getCategory :: Query -> Either Error Query
getCategory limitOffset =
  Right $
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

parseCategory :: [T.Text] -> Either Error Category
parseCategory [categoryIdTxt, parentCategory, nameCategory] = do
  let idCategory = readNum categoryIdTxt
  if idCategory == 0
    then Left $ ParseError ParseCategoryError
    else pure $ Category idCategory parentCategory nameCategory
parseCategory _ = Left $ ParseError ParseCategoryError

newtype CategoryHandle m = CategoryHandle
  {checkUniqCategoryH :: BC.ByteString -> m Bool}

categoryHandler :: CategoryHandle IO
categoryHandler = CategoryHandle {checkUniqCategoryH = checkUniqCategory}

getParentCategories :: T.Text -> IO [T.Text]
getParentCategories category = do
  conn <- connectDB
  allCategoriesLs <- query_ conn getCategory' :: IO [[T.Text]]
  close conn
  writingLineDebug allCategoriesLs
  let buildingList categoryLs = do
        let val =
              LT.find
                (\(_ : nameCategory : _) -> pure nameCategory == listToMaybe categoryLs)
                allCategoriesLs
        case val of
          Just (parentCategory : _) -> buildingList (parentCategory : categoryLs)
          _ -> categoryLs
  pure $ filter (/= "Null") $ buildingList [category]

-- Request example:
--  '.../category?aaa>bbb'
--      aaa - parent category's name,
--      bbb - category's name.
createCategory ::
  Monad m =>
  CategoryHandle m ->
  m Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
createCategory CategoryHandle {..} isAdmin ls = do
  isAdm <- isAdmin
  if not isAdm || null ls || null categorys
    then pure $ Left CommonError
    else do
      isUniqName <- checkUniqCategoryH nameCategory
      isUniqParent <- checkUniqCategoryH parentCategory
      pure $ checkAndResponse isUniqName isUniqParent
  where
    (x : _) = map (BC.split '>' . fst) ls
    categorys = filter (/= "") x
    (parentCategory : nameCategory : _) =
      if length categorys == 1 then "Null" : categorys else categorys
    checkAndResponse isUniqName isUniqParent
      | not isUniqName = Left $ CategoryError CategoryExists
      | isUniqParent && parentCategory /= "Null" = Left $ CategoryError NoParentCategory
      | otherwise =
        Right $
          Query $
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
editCategory ::
  Monad m =>
  CategoryHandle m ->
  m Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
editCategory CategoryHandle {..} isAdmin ls = do
  isAdm <- isAdmin
  if not isAdm || null ls || null filteredLs || "" `elem` [name, new_name]
    then pure $ Left CommonError
    else do
      isUniqName <- checkUniqCategoryH name
      if isUniqName
        then pure $ Left $ CategoryError NoCategory
        else do
          isUniqNew_name <- checkUniqCategoryH new_name
          pure $ checkAndResponse isUniqNew_name
  where
    filteredLs = filter ((/= "") . snd) $ map (fmap (fromMaybe "")) $ take 1 ls
    splitedFilteredLs = map (fmap (BC.split '>')) filteredLs
    categories = map (filter (/= "") <$>) splitedFilteredLs
    methodAndNames@((method, [name, new_name]) : _) =
      map
        ( \x ->
            if length (snd x) /= 2
              then ("404", ["", ""])
              else x
        )
        categories
    checkAndResponse isUniq
      | method == "change_parent" && name == new_name = Left $ CategoryError CategoryParentItself
      | method == "change_name" && not isUniq = Left $ CategoryError CategoryExists
      | method == "change_parent" && isUniq && new_name /= "Null" = Left $ CategoryError NoParentCategory
      | otherwise = checkQuery $ map buildQuery methodAndNames
    checkQuery lq = if "404" `elem` lq then Left CommonError else Right . Query $ mconcat lq
    buildQuery (meth, [nameCategory, newNameCategory]) =
      case meth of
        "change_name" ->
          "UPDATE category \
          \ SET   parent_category = '"
            <> newNameCategory
            <> "' \
               \ WHERE parent_category = '"
            <> nameCategory
            <> "'; \
               \ UPDATE category \
               \ SET   name_category = '"
            <> newNameCategory
            <> "' \
               \ WHERE name_category = '"
            <> nameCategory
            <> "'; "
        "change_parent" ->
          "UPDATE category \
          \ SET parent_category = '"
            <> newNameCategory
            <> "' \
               \ WHERE name_category = '"
            <> nameCategory
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
