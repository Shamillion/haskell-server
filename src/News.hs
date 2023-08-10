{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module News where

import Auth (authorID, isAuthor)
import Category (getParentCategories)
import Config (connectDB)
import Control.Applicative (liftA2)
import Control.Exception (throwIO)
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
import Error (Error (CommonError, ParseError), ParseError (ParseNewsError))
import GHC.Generics (Generic)
import Lib
  ( LimitAndOffsetHandle (..),
    initTxt,
    limitAndOffsetHandler,
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
getNews :: W.Request -> Maybe (Query, Query) -> IO Query
getNews _ Nothing = throwIO CommonError
getNews req parametersForNews = do
  author <- authorID req
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

parseNews :: [T.Text] -> IO News
parseNews [newsIdTxt, title, creation_date, authorTxt, categoryTxt, content, photoTxt, isPublishedTxt] = do
  let news_id = readNum newsIdTxt
      splitText = splitOnTxt "," . tailTxt . initTxt
      eitherAuthor = parseUser $ splitText authorTxt
      photo = map ("/photo?get_photo=" <>) $ splitText photoTxt
      is_published = isPublishedTxt == "true" || isPublishedTxt == "t"
  if news_id == 0
    then throwIO $ ParseError ParseNewsError
    else do
      case eitherAuthor of
        Left err -> throwIO err
        Right author -> do
          categories <- getParentCategories categoryTxt
          pure $ News news_id title creation_date author categories content photo is_published
parseNews _ = throwIO $ ParseError ParseNewsError

setMethodNews ::
  Monad m =>
  LimitAndOffsetHandle m ->
  [(T.Text, Maybe T.Text)] ->
  m (Maybe (Query, Query))
setMethodNews LimitAndOffsetHandle {..} ls = do
  setLimOffs <- setLimitAndOffset LimitAndOffsetHandle {..} ls
  let sortNewsLimitOffset = fmap (<> setLimOffs) sortNews
  pure $ do
    filterForNews <- filterNews
    sortAndLimitAndOffset <- sortNewsLimitOffset
    pure (filterForNews, sortAndLimitAndOffset)
  where
    filterNews = setFiltersNews $ LT.filter ((`notElem` fields) . fst) ls
    fields = ["sort_by", "limit", "offset"]
    sortNews =
      case findSort of
        [("sort_by", Just param)] -> sortBy param
        _ -> pure ""
    findSort = LT.filter ((== "sort_by") . fst) ls

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
createNews :: IO Bool -> IO Query -> [(BC.ByteString, Maybe BC.ByteString)] -> IO Query
createNews isAuth authID ls = do
  isAuth' <- isAuth
  if not isAuth' || null ls || nothingInLs
    then throwIO CommonError
    else do
      authID' <- authID
      photoIdList <- photoIDLs ls
      let maybeStr =
            foldr
              (liftA2 (<>))
              (Just "")
              [ pure
                  "INSERT INTO news (title, creation_date, user_id, category_id, photo, \
                  \ content, is_published) \
                  \ VALUES ('",
                titleNws,
                pure "', NOW(), ",
                pure $ fromQuery authID',
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
      case maybeStr of
        Just str -> pure $ Query str
        Nothing -> throwIO CommonError
  where
    nothingInLs = any (\(_, y) -> isNothing y) ls
    titleNws = getValue "title"
    categoryId = getValue "category_id"
    contentNws = getValue "content"
    isPublished = getValue "is_published"
    getValue str = LT.find (\(x, _) -> x == str) ls >>= snd

-- Puts the photos from the query into the database and
--  returns a list from the ID.
photoIDLs :: [(BC.ByteString, Maybe BC.ByteString)] -> IO BC.ByteString
photoIDLs ls = idLs <&> (\x -> "'{" <> buildPhotoIdString x <> "}'")
  where
    idLs = mapM (sendPhotoToDB . fromMaybe "" . snd) . filter ((== "photo") . fst) $ ls

buildPhotoIdString :: [BC.ByteString] -> BC.ByteString
buildPhotoIdString [] = ""
buildPhotoIdString [x] = x
buildPhotoIdString (x : xs) = x <> ", " <> buildPhotoIdString xs

-- Request example:
--    news?news_id=(id news needed to edit)&title=Text&category_id=3&
--       content=Text&photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
editNews :: W.Request -> [(BC.ByteString, Maybe BC.ByteString)] -> IO Query
editNews req ls = do
  authId <- authorID req
  isAuth <- authorNews (fromQuery authId) newsId
  if null ls || not isAuth
    then throwIO CommonError
    else do
      photoIdStr <- photoIdList
      let maybeStr =
            buildChanges filteredLs >>= \str ->
              pure $
                "UPDATE news SET " <> photoIdStr <> str
                  <> " WHERE news_id = "
                  <> newsId
                  <> ";"
      case maybeStr of
        Just str -> pure $ Query str
        Nothing -> throwIO CommonError
  where
    newsId = case LT.find (\(x, _) -> x == "news_id") ls of
      Just (_, Just n) -> n
      _ -> "0"
    filteredLs = LT.filter (\(x, y) -> elem x fields && isJust y) ls
    fields = ["title", "category_id", "content", "is_published"]
    photoIdList =
      if "photo" `elem` map fst ls
        then do
          str <- photoIDLs ls
          pure $ "photo = " <> str <> ", "
        else pure ""

-- Checks whether this user is the author of this news.
authorNews :: BC.ByteString -> BC.ByteString -> IO Bool
authorNews "Null" _ = pure False
authorNews authId newsId = do
  conn <- connectDB
  ls <-
    query
      conn
      "SELECT news_id FROM news WHERE user_id = ? AND \
      \ news_id = ?;"
      (authId, newsId) ::
      IO [[Int]]
  close conn
  pure $ ls /= []

-- Creates a row with updated news fields.
buildChanges :: [(BC.ByteString, Maybe BC.ByteString)] -> Maybe BC.ByteString
buildChanges [] = Nothing
buildChanges [(field, maybeParam)] = maybeParam >>= \param -> pure $ field <> " = " <> q <> param <> q
  where
    q = if field `elem` fields then "'" else ""
    fields = ["title", "content"]
buildChanges ((field, maybeParam) : xs) = buildChanges [(field, maybeParam)] <> pure ", " <> buildChanges xs

getNewsHandler :: W.Request -> IO LC.ByteString
getNewsHandler req = do
  queryNews <- mkGetNewsQuery req 
  newsTxt <- runGetQuery queryNews :: IO [[T.Text]]
  encodeNews newsTxt 

mkGetNewsQuery :: W.Request -> IO Query
mkGetNewsQuery req = method >>= getNews req
  where
    method = setMethodNews limitAndOffsetHandler . queryToQueryText $ W.queryString req

encodeNews :: [[T.Text]] -> IO LC.ByteString
encodeNews = fmap encode . mapM parseNews

mkCreateNewsQuery :: W.Request -> IO Query
mkCreateNewsQuery req = createNews athr authId arr
  where
    athr = isAuthor req
    authId = authorID req
    arr = W.queryString req

mkEditNewsQuery :: W.Request -> IO Query
mkEditNewsQuery req = editNews req $ W.queryString req
