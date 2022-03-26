{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module News where


import Data.Aeson
import Data.List as LT                 (find) 
import Database.PostgreSQL.Simple
import Data.Monoid                     ((<>))
import qualified Data.Text as T hiding (last)
import Data.Text.Encoding              (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Generics
import System.IO.Unsafe                (unsafePerformIO)
import Author
import Db



getNews :: Query -> Query
getNews str = 
  "SELECT DISTINCT substring(title from 1 for 12), (news.creation_date :: TEXT), \
   \ (author_ :: TEXT), name_category, (ARRAY_AGG(DISTINCT name_tag) :: TEXT), \
   \ substring(content from 1 for 12), main_photo, (photo :: TEXT) \
   \ FROM news_publish \    
   \ INNER JOIN news ON news_publish.news_id = news.news_id \   
   \ INNER JOIN tag ON news_publish.tag_id = tag.tag_id \
   \ INNER JOIN category ON news.category_id = category.category_id \
   \ INNER JOIN ( SELECT  author.author_id, name_user, surname_user, avatar, \
   \ users.creation_date, is_admin, description_author \
   \ FROM news \
   \ INNER JOIN  author ON news.author_id = author.author_id \  
   \ INNER JOIN  users ON users.user_id  = author.user_id \
   \ ) author_ ON news.author_id = author_.author_id "                        
   <> str <>  -- WHERE title LIKE '%' 
   " GROUP BY title, news.creation_date, author_, name_category, \
   \ main_photo, photo, content;"

data News = News
  { title         :: T.Text
  , creation_date :: T.Text
  , author        :: Author
  , category      :: [T.Text] 
  , tags          :: [T.Text] 
  , content       :: T.Text
  , main_photo    :: T.Text
  , photo         :: [T.Text]  
  }
   deriving (Show, Generic, ToJSON)

errorNews :: News
errorNews =  News "error" "error" errorAuthor ["error"] 
                         ["error"] "error" "error" ["error"]   

parseNews :: [T.Text] -> News
parseNews ls
  | length ls /= 8 = errorNews
  | otherwise = News n1 n2 author cats tags n6 n7 pht 
  where 
    [n1,n2,n3,n4,n5,n6,n7,n8] = ls
    splitText = T.splitOn "," . T.tail . T.init 
    author = parseAuthor $ splitText n3
    cats = getParentCategories n4
    pht = splitText n8
    tags = splitText n5

getParentCategories :: T.Text -> [T.Text] 
getParentCategories cat = unsafePerformIO $ do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='serverdb' \
                             \ user='haskell' password='haskell'"
  ls <- query_ conn $ getCategory :: IO [[T.Text]]
  let buildingList pc = do
        let val = LT.find (\(x:y:xy) -> y == head pc) ls 
        case val of
          Just el -> buildingList (head el : pc)
          _       -> pc 
  pure $ filter (/="Null") $ buildingList [cat]

setMethodNews :: [(T.Text, Maybe T.Text)] -> Query     
setMethodNews ls = " WHERE " <> "title LIKE '%'"
  --case mthd of
  --"created_at" -> "creation_date = '" <> fromMaybe param <> "'" 
  --_            -> "title LIKE '%'"
  --where
    --fromMaybe (Just e) = e
    --fromMaybe Nothing  = "Null"











