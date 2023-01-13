{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (SpecWith, it, shouldBe, hspec, describe)
import Lib  (readNum)
import TestsFunctionsNews
import TestsFunctionsCategory

testsFunctionReadNum :: SpecWith ()
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
  describe "Check function readNum"      testsFunctionReadNum
  describe "Check setLimitAndOffsetWith" testsFunctionSetLimitAndOffsetWith
  describe "Check setMethodNews"         testsFunctionSetMethodNews
  describe "Check createCategoryWith"    testsFunctionCreateCategoryWith 
  describe "Check editCategoryWith"      testsFunctionEditCategoryWith      
        
        
        
            
      
      
      
      
      
      
      
      
      
      
      

