{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module News where


import Data.Aeson
import Data.List as LT               (find, filter) 
import Data.String                   (fromString)
import Database.PostgreSQL.Simple as S
import Database.PostgreSQL.Simple.Types
import Data.Monoid                     ((<>))
import qualified Data.Text as T 
import Data.Text.Encoding            (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8      as BC
import GHC.Generics
import System.IO.Unsafe                (unsafePerformIO)
import User
import Category
import Photo
-- import Debug.Trace
import Config
import Lib



getNews :: Query -> Maybe (Query, Query) -> Query
getNews auth str = if str == Nothing 
  then "404"
  else
  "SELECT (news_id :: TEXT), substring(title from 1 for 12), \
   \ (news.creation_date :: TEXT), (author :: TEXT), \ 
   \ name_category, substring(content from 1 for 12), \ 
   \ (photo :: TEXT), (is_published :: TEXT) \
   \ FROM news \ 
   \ INNER JOIN category ON news.category_id = category.category_id \
   \ INNER JOIN ( SELECT  user_id, name_user, users.creation_date, is_admin, \
   \ is_author, login \
   \ FROM users ) author ON news.user_id = author.user_id \                        
   \ WHERE (is_published = TRUE OR is_published = FALSE AND \
   \ author.user_id = " <> auth <> ") AND " <> fltr <>  -- WHERE title LIKE '%' 
   " GROUP BY news_id, title, news.creation_date, author, author.name_user, \
   \ name_category, photo, content, is_published "  <> srt <> ";"
   where 
     Just (fltr,srt) = str

data News = News
  { news_id       :: Int
  , title         :: T.Text
  , creation_date :: T.Text
  , author        :: User
  , category      :: [T.Text]   
  , content       :: T.Text  
  , photo         :: [T.Text]  
  , is_published  :: Bool
  }
   deriving (Show, Generic, ToJSON)

errorNews :: News
errorNews =  News 0 "error" "error" errorUser ["error"] "error" ["error"] False  

parseNews :: [T.Text] -> News
parseNews ls
  | length ls /= 8 = errorNews
  | idNws == 0 = errorNews
  | otherwise = News idNws n1 n2 author cats n5 pht isPbl 
  where 
    [n0,n1,n2,n3,n4,n5,n6,n7] = ls
    idNws = readNum n0
    splitText = splitOnTxt "," . tailTxt . initTxt 
    author = parseUser $ splitText n3
    cats = getParentCategories n4
    pht = map ("/photo?get_photo=" <>) $ splitText n6
    isPbl = n7 == "true" || n7 == "t" 
    

setMethodNews :: [(T.Text, Maybe T.Text)] -> Maybe (Query, Query)
setMethodNews ls = do
  a <- filterNews
  b <- sortNewsLimitOffset
  pure (a,b)
  where
    filterNews = setFiltersNews $ LT.filter ((`notElem` fields) . fst) ls
    fields = ["sort_by", "limit", "offset"]
    sortNews = 
      case findSort of
        [("sort_by", Just x)] -> sortBy x
        _ -> pure ""
    findSort = LT.filter ((=="sort_by") . fst) ls
    sortNewsLimitOffset = fmap (<> setLimitAndOffset ls) sortNews

sortBy :: T.Text -> Maybe Query 
sortBy mthd = ("ORDER BY " <>) <$>
  case mthd of   
   "date"     -> Just $ "creation_date"
   "author"   -> Just $ "author.name_user"   
   "category" -> Just $ "name_category"
   "photo"    -> Just $ "CARDINALITY(photo)" 
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
        "author"   -> Just $ "name_user = '" <> fromMaybe' param <> "'" 
        "category" -> Just $ "News.category_id = " <> fromMaybe' param
        "title"    -> Just $ "title ILIKE '%" <> fromMaybe' param <> "%'" 
        "content"  -> Just $ "content ILIKE '%" <> fromMaybe' param <> "%'" 
        "search"   -> Just $ "(content || name_user || name_category) ILIKE '%"  
                                                     <> fromMaybe' param <> "%'" 
        _ -> Nothing 
    fromMaybe' = fromString . T.unpack . fromMaybe            
    creationDate x = "News.creation_date " <> x <> " '" <> 
                                              fromMaybe' param <> "'"
  
setLimitAndOffset :: [(T.Text, Maybe T.Text)] -> Query 
setLimitAndOffset ls = Query $ " LIMIT " <> lmt <> " OFFSET " <> ofst 
  where
    ofst = case LT.find ((== "offset") . fst) ls of
               Just (_, Just n) -> encodeUtf8 n
               _                -> "0"
    lmt  = case LT.find ((== "limit") . fst) ls of
               Just (_, Just n) -> ordNum n
               _                -> num
    num  = BC.pack $ show limitElem
    ordNum x = 
      if (read $ T.unpack x) > limitElem then num else encodeUtf8 x
    

--'.../news?title=Text&category_id=3&content=Text&
--       photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
createNews :: Bool -> Query -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
createNews False _ _ = "404"
createNews _ _ [] = "404"
createNews True auth ls 
  | nothingInLs = "404" 
  | otherwise = Query $
      "INSERT INTO news (title, creation_date, user_id, category_id, photo, \ 
      \ content, is_published) \
      \ VALUES ('" <> title <> "', NOW(), " <> fromQuery auth <> ", " <> 
        categoryId <> ", " <> photoIDLs ls' <> ", '" <> content <> 
        "', " <> isPublished <> ");"
  where
    nothingInLs = any (\(x,y) -> y == Nothing) ls 
    ls' = map (fromMaybe <$>) ls
    title = getValue "title"
    categoryId = getValue "category_id"     
    content = getValue "content"
    isPublished = getValue "is_published"
    getValue str = sndMaybe . LT.find (\(x,y) -> x == str) $ ls'
    sndMaybe Nothing  = "Null"
    sndMaybe (Just e) = snd e
    

photoIDLs :: [(BC.ByteString, BC.ByteString)] -> BC.ByteString 
photoIDLs = (\x -> "'{" <> x <> "}'") . buildPhotoIdString . 
                         map (sendPhotoToDB . snd) . filter ((=="photo") . fst) 

buildPhotoIdString :: [BC.ByteString] -> BC.ByteString    
buildPhotoIdString [] = ""
buildPhotoIdString [x] = x
buildPhotoIdString (x:xs) = x <> ", " <> buildPhotoIdString xs
    
    
    --news?news_id=(id news needed to edit)&title=Text&category_id=3&
--       content=Text&photo=data%3Aimage%2Fpng%3Bbase64%2CaaaH..&
--          photo=data%3Aimage%2Fpng%3Bbase64%2CcccHG..&is_published=false'
editNews :: Query -> [(BC.ByteString, Maybe BC.ByteString)] -> Query
editNews _ [] = "404"
editNews auth ls 
   | not author = "404"
   | otherwise = Query $ "UPDATE news SET " <>  photo' <> buildChanges ls' <> 
                         " WHERE news_id = " <> newsId <> ";"
  where
    author = authorNews (fromQuery auth) newsId
    newsId = case LT.find (\(x,y) -> x == "news_id") ls of
               Just (_, Just n) -> n
               _                -> "0" 
    ls' = map (fromMaybe <$>) $ 
               LT.filter (\(x,y) -> elem x fields && y /= Nothing) ls    
    fields = ["title", "category_id", "content", "is_published"]
    photo' = if elem "photo" (map fst ls) 
               then "photo = " <> photoIDLs (map (fromMaybe <$>) ls) <> ", " 
               else ""   
  
  
authorNews :: BC.ByteString -> BC.ByteString -> Bool
authorNews authId newsId = 
  unsafePerformIO $ do 
    conn <- connectDB
    ls <- query conn  "SELECT news_id FROM news WHERE user_id = ? AND \
                                \ news_id = ?;" (authId , newsId) :: IO [[Int]] 
    pure $ ls /= []            
    
    
buildChanges :: [(BC.ByteString, BC.ByteString)] -> BC.ByteString    
buildChanges [] = ""
buildChanges [(x,y)] = x <> " = " <> q <> y <> q
  where 
    q = if elem x fields then "'" else "" 
    fields = ["title", "content"] 
buildChanges ((x,y):xs) = buildChanges [(x,y)] <> ", " <> buildChanges xs 

       
    
    
    
    
    
    



