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
getNews auth str =
  if isNothing str
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
          <> auth
          <> ") AND "
          <> fltr
          <> " GROUP BY news_id, title, news.creation_date, author, author.name_user, \
             \ name_category, photo, content, is_published "
          <> srt
          <> ";"
  where
    Just (fltr, srt) = str

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
parseNews ls
  | length ls /= 8 = pure . Left $ ParseError ParseNewsError
  | idNws == 0 = pure . Left $ ParseError ParseNewsError
  | otherwise = do
    cats <- getParentCategories n4
    pure $ eitherAuthor >>= \athr -> pure $ News idNws n1 n2 athr cats n5 pht isPbl
  where
    [n0, n1, n2, n3, n4, n5, n6, n7] = ls
    idNws = readNum n0
    splitText = splitOnTxt "," . tailTxt . initTxt
    eitherAuthor = parseUser $ splitText n3
    pht = map ("/photo?get_photo=" <>) $ splitText n6
    isPbl = n7 == "true" || n7 == "t"

setMethodNews ::
  Monad m =>
  NewsyHandle m ->
  [(T.Text, Maybe T.Text)] ->
  m (Maybe (Query, Query))
setMethodNews NewsyHandle {..} ls = do
  setLimOffs <- setLimitAndOffsetH ls
  let sortNewsLimitOffset = fmap (<> setLimOffs) sortNews
  pure $ do
    a <- filterNews
    b <- sortNewsLimitOffset
    pure (a, b)
  where
    filterNews = setFiltersNews $ LT.filter ((`notElem` fields) . fst) ls
    fields = ["sort_by", "limit", "offset"]
    sortNews =
      case findSort of
        [("sort_by", Just x)] -> sortBy x
        _ -> pure ""
    findSort = LT.filter ((== "sort_by") . fst) ls

-- Processing of the "sort_by=..." part of the request.
sortBy :: T.Text -> Maybe Query
sortBy mthd =
  ("ORDER BY " <>)
    <$> case mthd of
      "date" -> Just "creation_date"
      "author" -> Just "author.name_user"
      "category" -> Just "name_category"
      "photo" -> Just "CARDINALITY(photo)"
      _ -> Nothing

-- Processing parts of the request for filtering and searching.
setFiltersNews :: [(T.Text, Maybe T.Text)] -> Maybe Query
setFiltersNews [] = pure "title LIKE '%'"
setFiltersNews ((mthd, param) : xs)
  | null xs = choiceFilter
  | otherwise = choiceFilter >>= nextStep
  where
    nextStep n = ((n <> " AND ") <>) <$> setFiltersNews xs
    choiceFilter =
      case mthd of
        "created_at" -> creationDate "="
        "created_until" -> creationDate "<"
        "created_since" -> creationDate ">="
        "author" -> param' <&> (("name_user = '" <>) . (<> "'"))
        "category" -> ("News.category_id = " <>) <$> param'
        "title" -> param' <&> (("title ILIKE '%" <>) . (<> "%'"))
        "content" -> param' <&> (("content ILIKE '%" <>) . (<> "%'"))
        "search" -> param' <&> (("(content || name_user || name_category) ILIKE '%" <>) . (<> "%'"))
        _ -> Nothing
    param' = fromString . T.unpack <$> param
    creationDate x = (("News.creation_date " <> x <> " '") <>) <$> param' <&> (<> "'")

setLimitAndOffset ::
  Monad m =>
  NewsyHandle m ->
  [(T.Text, Maybe T.Text)] ->
  m Query
setLimitAndOffset NewsyHandle {..} ls = do
  val <- limitElemH
  let lmt = listToValue "limit" lessVal val
      lessVal x = if x > 0 && x < val then x else val
  pure . Query $ " LIMIT " <> lmt <> " OFFSET " <> ofst
  where
    ofst = listToValue "offset" (max 0) 0
    getMaybe wrd f =
      (LT.find ((== wrd) . fst) ls >>= snd >>= readMaybe . T.unpack) <&> f
    listToValue wrd f x = toByteString $
      case getMaybe wrd f of
        Just n -> n
        _ -> x
    toByteString = BC.pack . show

-- Request example:
-- '.../news?title=Text&category_id=3&content=Text&
--       photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
createNews :: IO Bool -> IO Query -> [(BC.ByteString, Maybe BC.ByteString)] -> IO (Either Error Query)
createNews auth authID ls = do
  auth' <- auth
  if not auth' || null ls || nothingInLs
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
photoIDLs ls = ls' <&> (\x -> "'{" <> buildPhotoIdString x <> "}'")
  where
    ls' = mapM (sendPhotoToDB . fromMaybe "" . snd) . filter ((== "photo") . fst) $ ls

buildPhotoIdString :: [BC.ByteString] -> BC.ByteString
buildPhotoIdString [] = ""
buildPhotoIdString [x] = x
buildPhotoIdString (x : xs) = x <> ", " <> buildPhotoIdString xs

-- Request example:
--    news?news_id=(id news needed to edit)&title=Text&category_id=3&
--       content=Text&photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
editNews :: IO Query -> [(BC.ByteString, Maybe BC.ByteString)] -> IO (Either Error Query)
editNews auth ls = do
  auth' <- auth
  author' <- authorNews (fromQuery auth') newsId
  if null ls || not author'
    then pure $ Left CommonError
    else do
      photo' <- photoIdList
      let maybeStr =
            buildChanges ls' >>= \str ->
              pure $
                "UPDATE news SET " <> photo' <> str
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
    ls' = LT.filter (\(x, y) -> elem x fields && isJust y) ls
    fields = ["title", "category_id", "content", "is_published"]
    photoIdList =
      if "photo" `elem` map fst ls
        then do
          x <- photoIDLs ls
          pure $ "photo = " <> x <> ", "
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
buildChanges [(x, y)] = y >>= \y' -> pure $ x <> " = " <> q <> y' <> q
  where
    q = if x `elem` fields then "'" else ""
    fields = ["title", "content"]
buildChanges ((x, y) : xs) = buildChanges [(x, y)] <> pure ", " <> buildChanges xs
