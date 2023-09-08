{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Category where

import Config (writingLineDebug)
import Control.Exception (throwIO)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List as LT (find)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error
  ( CategoryError (..),
    Error
      ( CategoryError,
        CommonError
      ),
  )
import GHC.Generics (Generic)
import Lib (limitAndOffsetHandler, runGetQuery, setLimitAndOffset)
import Network.HTTP.Types (queryToQueryText)
import qualified Network.Wai as W
import ConnectDB (connectDB)

-- Creating a database query to get a list of catygories.
mkGetCategoryQuery :: Query -> Query
mkGetCategoryQuery limitOffset =
  "SELECT category_id, parent_category, \
  \ name_category FROM category "
    <> limitOffset
    <> ";"

-- For getParentCategories.
getCategoryForList :: Query
getCategoryForList = "SELECT parent_category, name_category FROM category;"

data Category = Category
  { category_id :: Int,
    parent_category :: T.Text,
    name_category :: T.Text
  }
  deriving (Show, Generic, FromRow, ToJSON)

newtype CategoryHandle m = CategoryHandle
  {checkUniqCategoryH :: BC.ByteString -> m Bool}

categoryHandler :: CategoryHandle IO
categoryHandler = CategoryHandle {checkUniqCategoryH = checkUniqCategory}

getParentCategories :: T.Text -> IO [T.Text]
getParentCategories category = do
  conn <- connectDB
  allCategoriesLs <- query_ conn getCategoryForList :: IO [[T.Text]]
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
  Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
createCategory CategoryHandle {..} isAdmin ls = do
  if not isAdmin || null ls || null categorys
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
  Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
editCategory CategoryHandle {..} isAdmin ls = do
  if not isAdmin || null ls || null filteredLs || "" `elem` [name, new_name]
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
              then ("", ["", ""])
              else x
        )
        categories
    checkAndResponse isUniq
      | method == "change_parent" && name == new_name = Left $ CategoryError CategoryParentItself
      | method == "change_name" && not isUniq = Left $ CategoryError CategoryExists
      | method == "change_parent" && isUniq && new_name /= "Null" = Left $ CategoryError NoParentCategory
      | otherwise = Query . mconcat <$> mapM buildQuery methodAndNames
    buildQuery (meth, [nameCategory, newNameCategory]) =
      case meth of
        "change_name" ->
          pure $
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
          pure $
            "UPDATE category \
            \ SET parent_category = '"
              <> newNameCategory
              <> "' \
                 \ WHERE name_category = '"
              <> nameCategory
              <> "'; "
        _ -> Left CommonError
    buildQuery (_, _) = Left CommonError

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

getCategoryHandler :: W.Request -> IO LC.ByteString
getCategoryHandler req = do
  queryCategory <- buildGetCategoryQuery req
  category <- runGetQuery queryCategory :: IO [Category]
  pure $ encode category

buildGetCategoryQuery :: W.Request -> IO Query
buildGetCategoryQuery req = do
  limitOffset <- setLimitAndOffset limitAndOffsetHandler . queryToQueryText $ arr
  pure $ mkGetCategoryQuery limitOffset
  where
    arr = W.queryString req

buildCreateOrEditCategoryQuery ::
  ( CategoryHandle IO ->
    Bool ->
    [(BC.ByteString, Maybe BC.ByteString)] ->
    IO (Either Error Query)
  ) ->
  Bool ->
  W.Request ->
  IO Query
buildCreateOrEditCategoryQuery func isAdmin req = do
  eitherQuery <- func categoryHandler isAdmin $ W.queryString req
  case eitherQuery of
    Left err -> throwIO err
    Right qry -> pure qry

buildCreateCategoryQuery :: Bool -> W.Request -> IO Query
buildCreateCategoryQuery = buildCreateOrEditCategoryQuery createCategory

buildEditCategoryQuery :: Bool -> W.Request -> IO Query
buildEditCategoryQuery = buildCreateOrEditCategoryQuery editCategory
