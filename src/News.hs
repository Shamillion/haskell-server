{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module News where


import Data.Aeson
import Data.List as LT               (find, filter) 
import Data.String                   (fromString)
import Database.PostgreSQL.Simple as S
import Data.Monoid                     ((<>))
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Generics
--import System.IO.Unsafe                (unsafePerformIO)
import User
import Category
-- import Debug.Trace



getNews :: Maybe (Query, Query) -> Query
getNews str = if str == Nothing 
  then "404"
  else
  "SELECT substring(title from 1 for 12), (news.creation_date :: TEXT), \
   \ (author :: TEXT), name_category, substring(content from 1 for 12), \ 
   \ (photo :: TEXT), (is_published :: TEXT) \
   \ FROM news \ 
   \ INNER JOIN category ON news.category_id = category.category_id \
   \ INNER JOIN ( SELECT  user_id, name_user, users.creation_date, is_admin, \
   \ is_author \
   \ FROM users ) author ON news.user_id = author.user_id \                        
   \ WHERE " <> fltr <>  -- WHERE title LIKE '%' 
   " GROUP BY title, news.creation_date, author, author.name_user, \
   \ name_category, photo, content, is_published "  <> srt
   where 
     Just (fltr,srt) = str

data News = News
  { title         :: T.Text
  , creation_date :: T.Text
  , author        :: User
  , category      :: [T.Text]   
  , content       :: T.Text  
  , photo         :: [T.Text]  
  , is_published  :: Bool
  }
   deriving (Show, Generic, ToJSON)

errorNews :: News
errorNews =  News "error" "error" errorUser ["error"] "error" ["error"] False  

parseNews :: [T.Text] -> News
parseNews ls
  | length ls /= 7 = errorNews
  | otherwise = News n1 n2 author cats n5 pht isPbl 
  where 
    [n1,n2,n3,n4,n5,n6,n7] = ls
    splitText = T.splitOn "," . T.tail . T.init 
    author = parseUser $ splitText n3
    cats = getParentCategories n4
    pht = map ("/photo?get_photo=" <>) $ splitText n6
    isPbl = n7 == "t"
    

setMethodNews :: [(T.Text, Maybe T.Text)] -> Maybe (Query, Query)
setMethodNews ls = do
  a <- filterNews
  b <- sortNews
  pure (a,b)
  where
    filterNews = setFiltersNews $ LT.filter ((/="sort_by") . fst) ls
    sortNews = 
      case findSort of
        [("sort_by", Just x)] -> sortBy x
        _ -> pure ";"
    findSort = LT.filter ((=="sort_by") . fst) ls

sortBy :: T.Text -> Maybe Query 
sortBy mthd = fromString <$> ("ORDER BY " <>) <$>
  case mthd of   
   "date"     -> Just $ "creation_date;"
   "author"   -> Just $ "author.name_user;"   
   "category" -> Just $ "name_category;"
   "photo"    -> Just $ "CARDINALITY(photo);" 
   _          -> Nothing



setFiltersNews :: [(T.Text, Maybe T.Text)] -> Maybe Query   
setFiltersNews [] = pure "title LIKE '%'" 
setFiltersNews ((mthd, param):xs) 
      | xs == [] = choiceFilter 
      | otherwise = choiceFilter >>= nextStep   
  where
    nextStep n = ((n <> " AND ") <>) <$> setFiltersNews xs
    choiceFilter =
      case mthd of
        "created_at"    -> Just $ creationDate "="
        "created_until" -> Just $ creationDate "<"
        "created_since" -> Just $ creationDate ">="
        "author"   -> Just $ "name_user = '" <> fromMaybe param <> "'" 
        "category" -> Just $ "News.category_id = " <> fromMaybe param
        "title"    -> Just $ "title ILIKE '%" <> fromMaybe param <> "%'" 
        "content"  -> Just $ "content ILIKE '%" <> fromMaybe param <> "%'" 
        "search"   -> Just $ "(content || name_user || name_category) ILIKE '%"  
                                                      <> fromMaybe param <> "%'" 
        _ -> Nothing            
    fromMaybe (Just e) =  fromString $ T.unpack e
    fromMaybe Nothing  = "Null"
    creationDate x = "News.creation_date " <> x <> " '" <> fromMaybe param <> "'"










