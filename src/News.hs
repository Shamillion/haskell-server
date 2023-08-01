{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module News where

import Category (getParentCategories)
import Config (connectDB, limitElem)
import Control.Applicative (liftA2)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Char8 as BC
import Data.Functor ((<&>))
import Data.List as LT (filter, find)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String (fromString)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, query)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error (Error (CommonError, ParseError), ParseError (ParseNewsError))
import GHC.Generics (Generic)
import Lib (initTxt, readNum, splitOnTxt, tailTxt)
import Photo (sendPhotoToDB)
import Text.Read (readMaybe)
import User (User, parseUser)

-- Creating a database query to get a list of news.
getNews :: Query -> Maybe (Query, Query) -> Either Error Query
getNews author parametersForNews =
  if isNothing parametersForNews
    then Left CommonError
    else
      Right $
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

data NewsyHandle m = NewsyHandle
  { limitElemH :: m Int,
    setLimitAndOffsetH :: [(T.Text, Maybe T.Text)] -> m Query
  }

newsHandler :: NewsyHandle IO
newsHandler =
  NewsyHandle
    { limitElemH = limitElem,
      setLimitAndOffsetH = setLimitAndOffset newsHandler
    }

parseNews :: [T.Text] -> IO (Either Error News)
parseNews [newsIdTxt, title, creation_date, authorTxt, categoryTxt, content, photoTxt, isPublishedTxt] = do
  let news_id = readNum newsIdTxt
      splitText = splitOnTxt "," . tailTxt . initTxt
      eitherAuthor = parseUser $ splitText authorTxt
      photo = map ("/photo?get_photo=" <>) $ splitText photoTxt
      is_published = isPublishedTxt == "true" || isPublishedTxt == "t"
  if news_id == 0
    then pure . Left $ ParseError ParseNewsError
    else do
      categories <- getParentCategories categoryTxt
      pure $
        eitherAuthor >>= \author ->
          pure $
            News news_id title creation_date author categories content photo is_published
parseNews _ = pure . Left $ ParseError ParseNewsError

setMethodNews ::
  Monad m =>
  NewsyHandle m ->
  [(T.Text, Maybe T.Text)] ->
  m (Maybe (Query, Query))
setMethodNews NewsyHandle {..} ls = do
  setLimOffs <- setLimitAndOffsetH ls
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

setLimitAndOffset ::
  Monad m =>
  NewsyHandle m ->
  [(T.Text, Maybe T.Text)] ->
  m Query
setLimitAndOffset NewsyHandle {..} ls = do
  limitInFile <- limitElemH
  let limit = listToValue "limit" setLimit limitInFile
      setLimit val = if val > 0 && val < limitInFile then val else limitInFile
  pure . Query $ " LIMIT " <> limit <> " OFFSET " <> offset
  where
    offset = listToValue "offset" (max 0) 0
    getMaybe param func =
      (LT.find ((== param) . fst) ls >>= snd >>= readMaybe . T.unpack) <&> func
    listToValue param func defaultVal = toByteString $
      case getMaybe param func of
        Just val -> val
        _ -> defaultVal
    toByteString = BC.pack . show

-- Request example:
-- '.../news?title=Text&category_id=3&content=Text&
--       photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
createNews :: IO Bool -> IO Query -> [(BC.ByteString, Maybe BC.ByteString)] -> IO (Either Error Query)
createNews isAuth authID ls = do
  isAuth' <- isAuth
  if not isAuth' || null ls || nothingInLs
    then pure $ Left CommonError
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
        Just str -> pure . pure . Query $ str
        Nothing -> pure $ Left CommonError
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
editNews :: IO Query -> [(BC.ByteString, Maybe BC.ByteString)] -> IO (Either Error Query)
editNews authId ls = do
  authId' <- authId
  isAuth <- authorNews (fromQuery authId') newsId
  if null ls || not isAuth
    then pure $ Left CommonError
    else do
      photoIdStr <- photoIdList
      let maybeStr =
            buildChanges filteredLs >>= \str ->
              pure $
                "UPDATE news SET " <> photoIdStr <> str
                  <> " WHERE news_id = "
                  <> newsId
                  <> ";"
      pure $ case maybeStr of
        Just str -> Right $ Query str
        Nothing -> Left CommonError
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
