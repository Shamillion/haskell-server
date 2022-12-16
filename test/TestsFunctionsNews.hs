{-# LANGUAGE OverloadedStrings #-}

module TestsFunctionsNews 
  ( testsFunctionSetLimitAndOffsetWith
  , testsFunctionSetMethodNews
  ) 
where

import Test.Hspec
import Test.QuickCheck
import Database.PostgreSQL.Simple.Types
import News (setLimitAndOffsetWith, setMethodNews) 

testsFunctionSetLimitAndOffsetWith = do
  it "limit and offset have not set in request" $                                
    setLimitAndOffsetWith 20 [] `shouldBe` Query " LIMIT 20 OFFSET 0"  
  it "limit is less than 0, offset have not set in request" $                                
    setLimitAndOffsetWith 20 [("limit",Just "-5")] `shouldBe` 
      Query " LIMIT 20 OFFSET 0"    
  it "limit is less than 20, offset have not set in request" $                                
    setLimitAndOffsetWith 20 [("limit",Just "5")] `shouldBe` 
      Query " LIMIT 5 OFFSET 0" 
  it "limit is 20, offset have not set in request" $                                
    setLimitAndOffsetWith 20 [("limit",Just "25")] `shouldBe` 
      Query " LIMIT 20 OFFSET 0"    
  it "limit is more than 20, offset have not set in request" $                                
    setLimitAndOffsetWith 20 [("limit",Just "25")] `shouldBe` 
      Query " LIMIT 20 OFFSET 0" 
  it "limit is not a number, offset have not set in request" $                                
    setLimitAndOffsetWith 20 [("limit",Just "ABC")] `shouldBe` 
      Query " LIMIT 20 OFFSET 0"  
  it "limit and offset are not a number" $                                
    setLimitAndOffsetWith 20 [("limit",Just "ABC"),("offset",Just "-DF$")] 
      `shouldBe` Query " LIMIT 20 OFFSET 0"      
  it "limit and offset are less than 0" $                                
    setLimitAndOffsetWith 20 [("limit",Just "-5"),("offset",Just "-3")] 
      `shouldBe` Query " LIMIT 20 OFFSET 0"
  it "limit is more than 20, offset is 3" $                                
    setLimitAndOffsetWith 20 [("limit",Just "50"),("offset",Just "3")] `shouldBe` 
      Query " LIMIT 20 OFFSET 3" 
  it "offset is 5, limit have not set in request" $                                
    setLimitAndOffsetWith 20 [("offset",Just "5")] `shouldBe` 
      Query " LIMIT 20 OFFSET 5"
  it "offset is less than 0, limit have not set in request" $                                
    setLimitAndOffsetWith 20 [("offset",Just "-5")] `shouldBe` 
      Query " LIMIT 20 OFFSET 0" 
  it "offset is not a number, limit have not set in request" $                                
    setLimitAndOffsetWith 20 [("offset",Just "S")] `shouldBe` 
      Query " LIMIT 20 OFFSET 0"       

testsFunctionSetMethodNews = do                          
  it "list is empty" $                                
    setMethodNews 20 [] `shouldBe` Just ("title LIKE '%'"," LIMIT 20 OFFSET 0") 
  it "test 1" $                                
    setMethodNews 20 
      [("created_since",Just "2022-03-22"), ("author",Just "Ann")
      ,("sort_by",Just "author"), ("limit",Just "10")
      ,("offset",Just "1")
      ] 
      `shouldBe` 
         Just ("News.creation_date >= '2022-03-22' AND name_user = 'Ann'",
               "ORDER BY author.name_user LIMIT 10 OFFSET 1")
  it "test 2" $                                
    setMethodNews 20 
      [("created_since",Just "2022-03-22"), ("author",Just "Ann")
      ,("sort_by",Just "foto"), ("limit",Just "10")
      ,("offset",Just "1")
      ] `shouldBe` Nothing
  it "test 3" $                                
    setMethodNews 20 
      [("created_at",Just "2022-03-22"), ("author",Just "Sam")
      ,("category",Just "11"),("sort_by",Just "photo"), ("limit",Just "-1")
      ,("offset",Just "1")
      ] 
      `shouldBe` 
         Just ("News.creation_date = '2022-03-22' AND name_user = 'Sam' AND \
                \News.category_id = 11",
               "ORDER BY CARDINALITY(photo) LIMIT 20 OFFSET 1")      
  it "test 4" $                                
    setMethodNews 20 
      [("created_until",Just "2022-03-22"), ("author",Just "Violette")
      ,("category",Just "4"), ("search",Just "Hello World!")
      ,("sort_by",Just "date"), ("limit",Just "0")      
      ] 
      `shouldBe` 
         Just ("News.creation_date < '2022-03-22' AND name_user = 'Violette' \
                \AND News.category_id = 4 \
                \AND (content || name_user || name_category) \
                \ILIKE '%Hello World!%'",
               "ORDER BY creation_date LIMIT 20 OFFSET 0")            
  it "test 5" $                                
    setMethodNews 20 
      [("created_at",Just "2022-03-22"), ("avthor",Just "Sam")
      ,("category",Just "11"),("sort_by",Just "photo"), ("limit",Just "10")
      ,("offset",Just "1")
      ] `shouldBe` Nothing 
  it "test 6" $                                
    setMethodNews 20 
      [("created_until",Just "2022-03-22"), ("author", Nothing)
      ,("category",Just "4"), ("title",Just "Test")
      ,("content",Just "Just"), ("search",Just "Hello World!")
      ,("sort_by",Just "date"), ("limit",Just "0")      
      ] 
      `shouldBe` 
         Just ("News.creation_date < '2022-03-22' AND name_user = '???' \
                \AND News.category_id = 4 AND title ILIKE '%Test%' \
                \AND content ILIKE '%Just%' \
                \AND (content || name_user || name_category) \
                \ILIKE '%Hello World!%'",
               "ORDER BY creation_date LIMIT 20 OFFSET 0")   
