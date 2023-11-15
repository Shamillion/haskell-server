{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoints.Category where

import ConnectDB (connectDB)
import Control.Monad.Reader (join, liftIO)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List as LT (find)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, close, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Environment (Flow)
import Error
  ( CategoryError (..),
    Error
      ( CategoryError,
        CommonError
      ),
    throwError,
  )
import GHC.Generics (Generic)
import Lib (runGetQuery, setLimitAndOffset)
import Logger (writingLineDebug)
import Network.HTTP.Types (queryToQueryText)
import qualified Network.Wai as W

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

categoryHandler :: CategoryHandle Flow
categoryHandler = CategoryHandle {checkUniqCategoryH = checkUniqCategory}

getParentCategories :: T.Text -> Flow [T.Text]
getParentCategories category = do
  conn <- connectDB
  allCategoriesLs <- liftIO $ query_ conn getCategoryForList :: Flow [[T.Text]]
  liftIO $ close conn
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
--  '.../category/create?categoryName=aaa&parentName=bbb'
--      aaa - category's name,
--      bbb - parent category's name.
createCategory ::
  Monad m =>
  CategoryHandle m ->
  Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
createCategory CategoryHandle {..} isAdmin dataFromRequest = do
  if not isAdmin || null dataFromRequest || nameCategory `elem` ["Null", ""]
    then pure $ Left CommonError
    else do
      isUniqName <- checkUniqCategoryH nameCategory
      isUniqParent <- checkUniqCategoryH parentCategory
      pure $ checkAndResponse isUniqName isUniqParent
  where
    getValueFromTuplesLs val = fromMaybe "Null" . join . lookup val $ dataFromRequest
    [nameCategory, parentCategory] = map getValueFromTuplesLs ["categoryName", "parentName"]
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

data EditCategoryMethod = ChangeName | ChangeParent | ErrorMethod
  deriving (Eq)

-- Request examples:
--   Changing the category name: '.../category/update?categoryName=aaa&newCategoryName=bbb'
--      aaa - old category's name,
--      bbb - new category's name.
--   Changing the parent category: '.../category/update?categoryName=aaa&parentName=bbb'
--      aaa - category's name,
--      bbb - new parent category's name.
editCategory ::
  Monad m =>
  CategoryHandle m ->
  Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
editCategory CategoryHandle {..} isAdmin dataFromRequest = do
  if not isAdmin || null dataFromRequest || method == ErrorMethod || "" `elem` [name, newName]
    then pure $ Left CommonError
    else do
      isUniqName <- checkUniqCategoryH name
      if isUniqName
        then pure $ Left $ CategoryError NoCategory
        else do
          isUniqNew_name <- checkUniqCategoryH newName
          pure $ checkAndResponse isUniqNew_name
  where
    getValueFromTuplesLs val = fromMaybe "" . join . lookup val $ dataFromRequest
    method = case map fst dataFromRequest of
      ["categoryName", "newCategoryName"] -> ChangeName
      ["categoryName", "parentName"] -> ChangeParent
      _ -> ErrorMethod
    name = getValueFromTuplesLs "categoryName"
    newName = getValueFromTuplesLs $
      case method of
        ChangeName -> "newCategoryName"
        ChangeParent -> "parentName"
        _ -> ""
    checkAndResponse isUniq
      | method == ChangeParent && name == newName = Left $ CategoryError CategoryParentItself
      | method == ChangeName && not isUniq = Left $ CategoryError CategoryExists
      | method == ChangeParent && isUniq && newName /= "Null" = Left $ CategoryError NoParentCategory
      | otherwise = Query <$> buildQuery method name newName
    buildQuery meth nameCategory newNameCategory =
      case meth of
        ChangeName ->
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
        ChangeParent ->
          pure $
            "UPDATE category \
            \ SET parent_category = '"
              <> newNameCategory
              <> "' \
                 \ WHERE name_category = '"
              <> nameCategory
              <> "'; "
        _ -> Left CommonError

-- Checking the existence of the category in the database.
checkExistCategory :: BC.ByteString -> Flow Bool
checkExistCategory str = do
  conn <- connectDB
  categoryLs <-
    liftIO $
      query_ conn $
        Query $
          "SELECT name_category FROM category WHERE category_id = '"
            <> str
            <> "';" ::
      Flow [[BC.ByteString]]
  liftIO $ close conn
  writingLineDebug categoryLs
  pure $ categoryLs /= []

-- Checking the uniqueness of the category name in the database.
checkUniqCategory :: BC.ByteString -> Flow Bool
checkUniqCategory str = do
  conn <- connectDB
  categoryLs <-
    liftIO $
      query_ conn $
        Query $
          "SELECT name_category FROM category WHERE \
          \ name_category = '"
            <> str
            <> "';" ::
      Flow [[BC.ByteString]]
  writingLineDebug categoryLs
  pure $ null categoryLs

getCategoryHandler :: W.Request -> Flow LC.ByteString
getCategoryHandler req = do
  queryCategory <- buildGetCategoryQuery req
  category <- runGetQuery queryCategory :: Flow [Category]
  pure $ encode category

buildGetCategoryQuery :: W.Request -> Flow Query
buildGetCategoryQuery req = do
  limitOffset <- setLimitAndOffset $ queryToQueryText $ W.queryString req
  pure $ mkGetCategoryQuery limitOffset

buildCreateOrEditCategoryQuery ::
  ( CategoryHandle Flow ->
    Bool ->
    [(BC.ByteString, Maybe BC.ByteString)] ->
    Flow (Either Error Query)
  ) ->
  Bool ->
  W.Request ->
  Flow Query
buildCreateOrEditCategoryQuery func isAdmin req = do
  conn <- connectDB
  eitherQuery <- func categoryHandler isAdmin $ W.queryString req
  liftIO $ close conn
  case eitherQuery of
    Left err -> throwError err
    Right qry -> pure qry

buildCreateCategoryQuery :: Bool -> W.Request -> Flow Query
buildCreateCategoryQuery = buildCreateOrEditCategoryQuery createCategory

buildEditCategoryQuery :: Bool -> W.Request -> Flow Query
buildEditCategoryQuery = buildCreateOrEditCategoryQuery editCategory
