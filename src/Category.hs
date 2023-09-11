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
import Database.PostgreSQL.Simple (FromRow, close, query_, Connection)
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
import Environment (Environment)
import Control.Monad.Reader (ReaderT, liftIO)

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

newtype CategoryHandle a m = CategoryHandle
  {checkUniqCategoryH :: a -> BC.ByteString -> m Bool}

categoryHandler :: CategoryHandle Connection IO
categoryHandler = CategoryHandle {checkUniqCategoryH = checkUniqCategory}

getParentCategories :: T.Text -> ReaderT Environment IO [T.Text]
getParentCategories category = do
  conn <- connectDB
  liftIO $ do
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
  CategoryHandle a m ->
  a ->  
  Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
createCategory CategoryHandle {..} conn isAdmin ls = do
  if not isAdmin || null ls || null categorys
    then pure $ Left CommonError
    else do
      isUniqName <- checkUniqCategoryH conn nameCategory
      isUniqParent <- checkUniqCategoryH conn parentCategory
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
  CategoryHandle a m ->
  a ->  
  Bool ->
  [(BC.ByteString, Maybe BC.ByteString)] ->
  m (Either Error Query)
editCategory CategoryHandle {..} conn isAdmin ls = do
  if not isAdmin || null ls || null filteredLs || "" `elem` [name, new_name]
    then pure $ Left CommonError
    else do
      isUniqName <- checkUniqCategoryH conn name
      if isUniqName
        then pure $ Left $ CategoryError NoCategory
        else do
          isUniqNew_name <- checkUniqCategoryH conn new_name
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
checkUniqCategory :: Connection -> BC.ByteString -> IO Bool
checkUniqCategory conn str = do
  ls <-
    query_ conn $
      Query $
        "SELECT name_category FROM category WHERE \
        \ name_category = '"
          <> str
          <> "';" ::
      IO [[BC.ByteString]]  
  writingLineDebug ls
  pure $ null ls

getCategoryHandler :: W.Request -> ReaderT Environment IO LC.ByteString
getCategoryHandler req = do
  queryCategory <- liftIO $ buildGetCategoryQuery req
  category <- runGetQuery queryCategory :: ReaderT Environment IO [Category]
  pure $ encode category

buildGetCategoryQuery :: W.Request -> IO Query
buildGetCategoryQuery req = do
  limitOffset <- setLimitAndOffset limitAndOffsetHandler . queryToQueryText $ arr
  pure $ mkGetCategoryQuery limitOffset
  where
    arr = W.queryString req

buildCreateOrEditCategoryQuery ::
  ( CategoryHandle Connection IO ->
    Connection -> 
    Bool ->
    [(BC.ByteString, Maybe BC.ByteString)] ->
    IO (Either Error Query)
  ) ->
  Bool ->
  W.Request ->
  ReaderT Environment IO Query
buildCreateOrEditCategoryQuery func isAdmin req = do
  conn <- connectDB
  liftIO $ do
    eitherQuery <- func categoryHandler conn isAdmin $ W.queryString req
    close conn
    case eitherQuery of
      Left err -> throwIO err
      Right qry -> pure qry

buildCreateCategoryQuery :: Bool -> W.Request -> ReaderT Environment IO Query
buildCreateCategoryQuery = buildCreateOrEditCategoryQuery createCategory

buildEditCategoryQuery :: Bool -> W.Request -> ReaderT Environment IO Query
buildEditCategoryQuery = buildCreateOrEditCategoryQuery editCategory
