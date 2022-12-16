{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Database.PostgreSQL.Simple.Types 
import Lib  (readNum)
import TestsFunctionsNews
import TestsFunctionsCategory

testsFunctionReadNum = do
  it "turns the Text to Int" $ do
    readNum "23" `shouldBe` (23 :: Int)
    readNum "-23" `shouldBe` ((-23) :: Int) 
    readNum "23.5" `shouldBe` (0 :: Int)     
    readNum "abc" `shouldBe` (0 :: Int) 
    readNum "23abc" `shouldBe` (0 :: Int)    
    readNum "abc23" `shouldBe` (0 :: Int)

                        
                        


main :: IO ()
main = hspec $ do
  --describe "Check function readNum"      testsFunctionReadNum
  describe "Check setLimitAndOffsetWith" testsFunctionSetLimitAndOffsetWith
  describe "Check setMethodNews"         testsFunctionSetMethodNews
  --describe "Check createCategoryWith"    testsFunctionCreateCategoryWith       
        
        
        
            
      
      
      
      
      
      
      
      
      
      
      

