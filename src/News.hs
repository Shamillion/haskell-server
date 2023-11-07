{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module News where

import Auth (authorID, isAuthor)
import Category (checkExistCategory, getParentCategories)
import ConnectDB (connectDB)
import Control.Applicative (liftA2)
import Control.Monad.Reader (ReaderT, liftIO)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor ((<&>))
import Data.List as LT (filter, find)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String (fromString)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, query)
import Database.PostgreSQL.Simple.Types (Query (..))
import Environment (Environment, Flow)
import Error (CategoryError (NoCategory), Error (CategoryError, CommonError, NewsError, ParseError), NewsError (..), ParseError (ParseNewsError), throwError)
import GHC.Generics (Generic)
import Lib
  ( initTxt,
    readNum,
    runGetQuery,
    setLimitAndOffset,
    splitOnTxt,
    tailTxt,
  )
import Network.HTTP.Types (queryToQueryText)
import qualified Network.Wai as W
import Photo (sendPhotoToDB)
import User (User, parseUser)

-- Creating a database query to get a list of news.
mkGetNewsQuery :: Query -> Maybe (Query, Query) -> Maybe Query
mkGetNewsQuery _ Nothing = Nothing
mkGetNewsQuery author parametersForNews =
  pure $
    "SELECT (news_id :: TEXT), title, (news.creation_date :: TEXT), \
    \ (author :: TEXT), name_category, content, (photo :: TEXT), \
    \ (is_published :: TEXT) \
    \ FROM news \
    \ INNER JOIN category ON news.category_id = category.category_id \
    \ INNER JOIN ( SELECT user_id, name_user, users.creation_date, is_admin, \
    \ is_author \
    \ FROM users ) author ON news.user_id = author.user_id \
    \ WHERE (is_published = TRUE OR is_published = FALSE AND \
    \ author.user_id = "
      <> author
      <> ") AND "
      <> filterForNews
      <> " GROUP BY news_id, title, news.creation_date, author, author.name_user, \
         \ name_category, photo, content, is_published "
      <> sortOptions
      <> ";"
  where
    Just (filterForNews, sortOptions) = parametersForNews

data News = News
  { news_id :: Int,
    title :: T.Text,
    creation_date :: T.Text,
    author :: User,
    category :: [T.Text],
    content :: T.Text,
    photo :: [T.Text],
    is_published :: Bool
  }
  deriving (Show, Generic, ToJSON)

parseNews :: [T.Text] -> Flow News
parseNews [newsIdTxt, title, creation_date, authorTxt, categoryTxt, content, photoTxt, isPublishedTxt] = do
  let news_id = readNum newsIdTxt
      splitText = splitOnTxt "," . tailTxt . initTxt
      eitherAuthor = parseUser $ splitText authorTxt
      photo = map ("/photo?get_photo=" <>) $ splitText photoTxt
      is_published = isPublishedTxt == "true" || isPublishedTxt == "t"
  if news_id == 0
    then throwError $ ParseError ParseNewsError
    else do
      case eitherAuthor of
        Left err -> throwError err
        Right author -> do
          categories <- getParentCategories categoryTxt
          pure $ News news_id title creation_date author categories content photo is_published
parseNews _ = throwError $ ParseError ParseNewsError

setMethodNews ::
  Monad m =>
  [(T.Text, Maybe T.Text)] ->
  ReaderT Environment m (Maybe (Query, Query))
setMethodNews dataFromRequest = do
  setLimOffs <- setLimitAndOffset dataFromRequest
  let sortNewsLimitOffset = fmap (<> setLimOffs) sortNews
  pure $ do
    filterForNews <- filterNews
    sortAndLimitAndOffset <- sortNewsLimitOffset
    pure (filterForNews, sortAndLimitAndOffset)
  where
    filterNews = setFiltersNews $ LT.filter ((`notElem` fields) . fst) dataFromRequest
    fields = ["sort_by", "limit", "offset"]
    sortNews =
      case findSort of
        [("sort_by", Just param)] -> sortBy param
        _ -> pure ""
    findSort = LT.filter ((== "sort_by") . fst) dataFromRequest

-- Processing of the "sort_by=..." part of the request.
sortBy :: T.Text -> Maybe Query
sortBy field =
  ("ORDER BY " <>)
    <$> case field of
      "date" -> Just "creation_date"
      "author" -> Just "author.name_user"
      "category" -> Just "name_category"
      "photo" -> Just "CARDINALITY(photo)"
      _ -> Nothing

-- Processing parts of the request for filtering and searching.
setFiltersNews :: [(T.Text, Maybe T.Text)] -> Maybe Query
setFiltersNews [] = pure "title LIKE '%'"
setFiltersNews ((field, param) : xs)
  | null xs = choiceFilter
  | otherwise = choiceFilter >>= nextStep
  where
    nextStep maybeQuery = ((maybeQuery <> " AND ") <>) <$> setFiltersNews xs
    choiceFilter =
      case field of
        "created_at" -> creationDate "="
        "created_until" -> creationDate "<"
        "created_since" -> creationDate ">="
        "author" -> argument <&> (("name_user = '" <>) . (<> "'"))
        "category" -> ("News.category_id = " <>) <$> argument
        "title" -> argument <&> (("title ILIKE '%" <>) . (<> "%'"))
        "content" -> argument <&> (("content ILIKE '%" <>) . (<> "%'"))
        "search" -> argument <&> (("(content || name_user || name_category) ILIKE '%" <>) . (<> "%'"))
        _ -> Nothing
    argument = fromString . T.unpack <$> param
    creationDate x = (("News.creation_date " <> x <> " '") <>) <$> argument <&> (<> "'")

-- Request example:
-- '.../news?title=Text&category_id=3&content=Text&
--       photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
mkCreateNewsQuery :: Query -> BC.ByteString -> [(BC.ByteString, Maybe BC.ByteString)] -> Maybe Query
mkCreateNewsQuery authID photoIdList dataFromRequest = do
  if null dataFromRequest || nothingInLs
    then Nothing
    else
      Query
        <$> foldr
          (liftA2 (<>))
          (Just "")
          [ pure
              "INSERT INTO news (title, creation_date, user_id, category_id, photo, \
              \ content, is_published) \
              \ VALUES ('",
            titleNws,
            pure "', NOW(), ",
            pure $ fromQuery authID,
            pure ", ",
            categoryId,
            pure ", ",
            pure photoIdList,
            pure ", '",
            contentNws,
            pure "', ",
            isPublished,
            pure ");"
          ]
  where
    nothingInLs = any (\(_, y) -> isNothing y) dataFromRequest
    titleNws = getValue "title"
    categoryId = getValue "category_id"
    contentNws = getValue "content"
    isPublished = getValue "is_published"
    getValue str = LT.find (\(x, _) -> x == str) dataFromRequest >>= snd

-- Puts the photos from the query into the database and
--  returns a list the ID.
buildPhotoIDLs :: [(BC.ByteString, Maybe BC.ByteString)] -> Flow BC.ByteString
buildPhotoIDLs dataFromRequest = photoIDLs <&> (\x -> "'{" <> mkPhotoIdString x <> "}'")
  where
    photoIDLs = mapM (sendPhotoToDB . fromMaybe "" . snd) . filter ((== "photo") . fst) $ dataFromRequest

mkPhotoIdString :: [BC.ByteString] -> BC.ByteString
mkPhotoIdString [] = ""
mkPhotoIdString [x] = x
mkPhotoIdString (x : xs) = x <> ", " <> mkPhotoIdString xs

-- Request example:
--    news?news_id=(id news needed to edit)&title=Text&category_id=3&
--       content=Text&photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
buildEditNewsQuery :: W.Request -> Flow Query
buildEditNewsQuery req = do
  authId <- authorID req
  isAuthorNews <- authorNews (fromQuery authId) newsId
  if not isAuthorNews
    then throwError $ NewsError NotAuthorThisNews
    else
      if null dataFromRequest
        then throwError CommonError
        else do
          photoIdStr <- photoIdList
          let maybeStr =
                mkUpdatedNewsFields filteredLs >>= \str ->
                  pure $
                    "UPDATE news SET " <> photoIdStr <> str
                      <> " WHERE news_id = "
                      <> newsId
                      <> ";"
          Query <$> getValueFromMaybe maybeStr
  where
    dataFromRequest = W.queryString req
    newsId = case LT.find (\(x, _) -> x == "news_id") dataFromRequest of
      Just (_, Just n) -> n
      _ -> "0"
    filteredLs = LT.filter (\(x, y) -> elem x fields && isJust y) dataFromRequest
    fields = ["title", "category_id", "content", "is_published"]
    photoIdList =
      if "photo" `elem` map fst dataFromRequest
        then do
          str <- buildPhotoIDLs dataFromRequest
          pure $ "photo = " <> str <> ", "
        else pure ""

getValueFromMaybe :: Maybe a -> Flow a
getValueFromMaybe maybeVal = case maybeVal of
  Just value -> pure value
  Nothing -> throwError CommonError

-- Checks whether this user is the author of this news.
authorNews :: BC.ByteString -> BC.ByteString -> Flow Bool
authorNews "Null" _ = pure False
authorNews authId newsId = do
  conn <- connectDB
  liftIO $ do
    newsIdLs <-
      query
        conn
        "SELECT news_id FROM news WHERE user_id = ? AND \
        \ news_id = ?;"
        (authId, newsId) ::
        IO [[Int]]
    close conn
    pure $ newsIdLs /= []

-- Creates a row with updated news fields.
mkUpdatedNewsFields :: [(BC.ByteString, Maybe BC.ByteString)] -> Maybe BC.ByteString
mkUpdatedNewsFields [] = Nothing
mkUpdatedNewsFields [(field, maybeParam)] = maybeParam >>= \param -> pure $ field <> " = " <> quote <> param <> quote
  where
    quote = if field `elem` fields then "'" else ""
    fields = ["title", "content"]
mkUpdatedNewsFields ((field, maybeParam) : xs) = mkUpdatedNewsFields [(field, maybeParam)] <> pure ", " <> mkUpdatedNewsFields xs

getNewsHandler :: W.Request -> Flow LC.ByteString
getNewsHandler req = do
  queryNews <- buildGetNewsQuery req
  newsTxt <- runGetQuery queryNews :: Flow [[T.Text]]
  encodeNews newsTxt

buildGetNewsQuery :: W.Request -> Flow Query
buildGetNewsQuery req = do
  author <- authorID req
  method <- setMethodNews $ queryToQueryText $ W.queryString req
  let maybeQuery = mkGetNewsQuery author method
  case maybeQuery of
    Just qry -> pure qry
    Nothing -> throwError CommonError

encodeNews :: [[T.Text]] -> Flow LC.ByteString
encodeNews = fmap encode . mapM parseNews

buildCreateNewsQuery :: W.Request -> Flow Query
buildCreateNewsQuery req = do
  athr <- isAuthor req
  if athr
    then do
      categoryExists <- checkExistCategory categoryId
      if categoryExists
        then do
          authId <- authorID req
          photoIdList <- buildPhotoIDLs dataFromRequest
          let maybeQuery = mkCreateNewsQuery authId photoIdList dataFromRequest
          getValueFromMaybe maybeQuery
        else throwError $ CategoryError NoCategory
    else throwError $ NewsError UserNotAuthor
  where
    dataFromRequest = W.queryString req
    categoryId = fromMaybe "0" $ LT.find (\(x, _) -> x == "category_id") dataFromRequest >>= snd
