{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Database.PostgreSQL.Simple.Types 
import Lib  (readNum)
import News (setLimitAndOffsetWith, setMethodNews) 


testsFunctionReadNum = do
  it "turns the Text to Int" $ do
    readNum "23" `shouldBe` (23 :: Int)    
    readNum "abc" `shouldBe` (0 :: Int) 
    readNum "23abc" `shouldBe` (0 :: Int)    
    readNum "abc23" `shouldBe` (0 :: Int)

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

testsFunctionSetMethodNews = do                          -- Int -> [(T.Text, Maybe T.Text)] -> Maybe (Query, Query)
  it "list is empty" $                                
    setMethodNews 20 [] `shouldBe` Just ("title LIKE '%'"," LIMIT 20 OFFSET 0") 


main :: IO ()
main = hspec $ do
  describe "Check function readNum"      testsFunctionReadNum
  describe "Check setLimitAndOffsetWith" testsFunctionSetLimitAndOffsetWith
  describe "Check setMethodNews"         testsFunctionSetMethodNews
         
        
        
        
            
      
      
      
      
      
      
      
      
      
      
      

