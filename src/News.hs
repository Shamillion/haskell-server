{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module News where


import Data.Aeson
import Data.List as LT                 (find, filter) 
import Data.String                   (fromString)
import Database.PostgreSQL.Simple as S
import Data.Monoid                     ((<>))
import qualified Data.Text as T hiding (last)
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Generics
import System.IO.Unsafe                (unsafePerformIO)
import User
import Db



getNews :: (Query, Query) -> Query
getNews str = 
  "SELECT substring(title from 1 for 12), (news.creation_date :: TEXT), \
   \ (author :: TEXT), name_category, substring(content from 1 for 12), \ 
   \ (photo :: TEXT), (is_published :: TEXT) \
   \ FROM news \ 
   \ INNER JOIN category ON news.category_id = category.category_id \
   \ INNER JOIN ( SELECT  user_id, name_user, users.creation_date, is_admin, \
   \ is_author \
   \ FROM users ) author ON news.user_id = author.user_id \                        
   \ WHERE " <> fst str <>  -- WHERE title LIKE '%' 
   " GROUP BY title, news.creation_date, author, author.name_user, \
   \ name_category, photo, content, is_published "  <> snd str

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
    

getParentCategories :: T.Text -> [T.Text] 
getParentCategories cat = unsafePerformIO $ do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='haskellserverlite' \
                             \ user='haskell' password='haskell'"
  ls <- query_ conn $ getCategory :: IO [[T.Text]]
  let buildingList pc = do
        let val = LT.find (\(x:y:xy) -> y == head pc) ls 
        case val of
          Just el -> buildingList (head el : pc)
          _       -> pc 
  pure $ filter (/="Null") $ buildingList [cat]


setMethodNews :: [(T.Text, Maybe T.Text)] -> (Query, Query)
setMethodNews ls = (filterNews, sortNews)
  where
    filterNews = setFiltersNews $ LT.filter ((/="sort_by") . fst) ls
    sortNews = 
      case findSort of
        [("sort_by", Just x)] -> sortBy x
        _ -> ";"
    findSort = LT.filter ((=="sort_by") . fst) ls

sortBy :: T.Text -> Query 
sortBy mthd = fromString $ "ORDER BY " <>
  case mthd of   
   "author"   -> "author.name_user;"   -- don't work
   "category" -> "name_category;"
   "photo"    -> "CARDINALITY(photo);" -- don't work
   _          -> "creation_date;"



setFiltersNews :: [(T.Text, Maybe T.Text)] -> Query   
setFiltersNews [] = "title LIKE '%'" 
setFiltersNews ((mthd, param):xs) 
      | xs == [] = choiceFilter 
      | otherwise = choiceFilter  <> " AND " <> setFiltersNews xs
  where
    choiceFilter =
      case mthd of
        "created_at"    -> creationDate "="
        "created_until" -> creationDate "<"
        "created_since" -> creationDate ">="
        "author"   -> "name_user = '" <> fromMaybe param <> "'" 
        "category" -> "News.category_id = " <> fromMaybe param
        "title"    -> "title ILIKE '%" <> fromMaybe param <> "%'" 
        "content"  -> "content ILIKE '%" <> fromMaybe param <> "%'" 
        "search"   -> "(content || name_user || name_category) ILIKE '%" <> 
                                                         fromMaybe param <> "%'" 
        _ -> "title LIKE '%'"            
    fromMaybe (Just e) =  fromString $ T.unpack e
    fromMaybe Nothing  = "Null"
    creationDate x = "News.creation_date " <> x <> " '" <> fromMaybe param <> "'"










