{-# LANGUAGE OverloadedStrings #-}

module TestsFunctionsCategory 
  ( testsFunctionCreateCategoryWith
  , testsFunctionEditCategoryWith
  ) 
where

import Data.ByteString.Internal         (ByteString) 
import Data.String                      (IsString)
import Database.PostgreSQL.Simple.Types (Query(..))
import Test.Hspec                       (SpecWith, it, shouldBe)
import Category                         (createCategoryWith, editCategoryWith)



testUniqCategory :: (Eq a, Data.String.IsString a) => a -> Bool
testUniqCategory e = e `notElem` ["parentCategory", "Null", "existCategory"]

createCategory' :: Bool -> [(ByteString, Maybe ByteString)] -> Query

createCategory' = createCategoryWith testUniqCategory

testsFunctionCreateCategoryWith :: SpecWith ()
testsFunctionCreateCategoryWith = do           
  it "User is not admin" $                               
    createCategory' False [("Cars>Wheels",Nothing)] `shouldBe` Query "404" 
  it "User is not admin and list is empty" $                               
    createCategory' False [] `shouldBe` Query "404"     
  it "User is admin and list is empty" $                               
    createCategory' True [] `shouldBe` Query "404" 
  it "Everything is all right" $                               
    createCategory' True [("parentCategory>category",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category', 'parentCategory');"
  it "'Just' instead 'Nothing'" $                               
    createCategory' True [("parentCategory>category",Just "anything")] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category', 'parentCategory');"                        
  it "Parent category does not exist" $                               
    createCategory' True [("notExistCategory>category",Nothing)] 
      `shouldBe` Query "406cp"
  it "Name of category is not unique" $                               
    createCategory' True [("parentCategory>existCategory",Nothing)] 
      `shouldBe` Query "406cu"
  it "Parent category does not exist and name of category is not unique" $                               
    createCategory' True [("notExistCategory>existCategory",Nothing)] 
      `shouldBe` Query "406cu"
  it "Name of category and name of parent category are equal" $                               
    createCategory' True [("parentCategory>parentCategory",Nothing)] 
      `shouldBe` Query "406cu"
  it "Name of category and name of parent category are equal and don't exist" $                               
    createCategory' True [("notExistCategory>notExistCategory",Nothing)] 
      `shouldBe` Query "406cp"
  it "Syntax mistake in request" $                               
    createCategory' True [("parentCategory<category",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('parentCategory<category', 'Null');"
  it "Syntax mistake in request - 2" $                               
    createCategory' True [("parentCategory>>category",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category', 'parentCategory');"                        
  it "Categories are more than 2" $                               
    createCategory' True [("parentCategory>category_1>category_2",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category_1', 'parentCategory');"                        
  it "2 elements into list" $                               
    createCategory' True 
      [ ("parentCategory>category_1",Nothing)
      , ("parentCategory>category_2",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category_1', 'parentCategory');" 
  it "3 elements into list" $                               
    createCategory' True 
      [ ("parentCategory>category_1",Nothing)
      , ("parentCategory>category_2",Nothing)
      , ("parentCategory>category_3",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category_1', 'parentCategory');" 
  it "Without parent category" $                               
    createCategory' True [(">category",Nothing)] 
      `shouldBe` Query "INSERT INTO category (name_category, parent_category) \
                        \ VALUES ('category', 'Null');" 
  it "Without category" $                               
    createCategory' True [("parentCategory>",Nothing)] 
      `shouldBe` Query "406cu"
  it "Without both categories" $                               
    createCategory' True [(">",Nothing)] 
      `shouldBe` Query "404" 

editCategory' :: Bool -> [(ByteString, Maybe ByteString)] -> Query
editCategory' = editCategoryWith testUniqCategory


testsFunctionEditCategoryWith :: SpecWith ()
testsFunctionEditCategoryWith = do                            -- [("change_name",Just "aaa>bbb")]
  it "User is not admin" $                               
    editCategory' False [("change_name",Just "existCategory>newCategory")] 
      `shouldBe` Query "404" 
  it "User is not admin and list is empty" $                               
    editCategory' False [] `shouldBe` Query "404"     
  it "User is admin and list is empty" $                               
    editCategory' True [] `shouldBe` Query "404" 
  it "Unknown method" $                               
    editCategory' True [("changeName",Just "existCategory>newCategory")] 
      `shouldBe` Query "404"     
  it "Without old name" $                               
    editCategory' True [("change_name",Just ">newCategory")] 
      `shouldBe` Query "404"
  it "Without new name" $                               
    editCategory' True [("change_name",Just "existCategory>")] 
      `shouldBe` Query "404"       
  it "Without both names" $                               
    editCategory' True [("change_name",Just ">")] 
      `shouldBe` Query "404"
  it "Syntax error" $                               
    editCategory' True [("change_name",Just "existeCategorynewCategory")] 
      `shouldBe` Query "404"      
  it "Syntax error 2" $                               
    editCategory' True [("change_name",Just "existCategory>>newCategory")] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existCategory'; "    
  it "With 3 category's name" $                               
    editCategory' True [( "change_name"
                        , Just "existCategory>newCategory>newCategory_2"
                        )] 
      `shouldBe` Query "404"                  
  it "With Nothing" $                               
    editCategory' True [("change_name",Nothing)] 
      `shouldBe` Query "404"
  it "Two elements in list. One element with syntax error" $                               
    editCategory' True [ ("change_name",Just "existCategory>newCategory")
                       , ("change_name",Just "existCategory>>newCategory_2") 
                       ] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existCategory'; "
  it "Two elements in list with Nothing" $                               
    editCategory' True [ ("change_name",Just "existCategory>newCategory")                       
                       , ("change_name",Nothing)
                       ] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existCategory'; "                      
  it "Method is change_name: such category already exists" $                               
    editCategory' True [("change_name",Just "parentCategory>existCategory")] 
      `shouldBe` Query "406cu"   
  it "Method is change_name: new name and old name are the same" $                               
    editCategory' True [("change_name",Just "existCategory>existCategory")] 
      `shouldBe` Query "406cu"
  it "Method is change_name: this category doesn't exist" $                               
    editCategory' True [("change_name",Just "someNameCategory>newCategory")] 
      `shouldBe` Query "406cn"      
  it "Method is change_name: everything is all right"$                               
    editCategory' True [("change_name",Just "existCategory>newCategory")] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existCategory'; "                            
  it "Method is change_parent: names of category and parent \
      \category are the same" $                               
    editCategory' True [("change_parent",Just "existCategory>existCategory")] 
      `shouldBe` Query "406ce"
  it "Method is change_parent: this category doesn't exist" $                               
    editCategory' True [( "change_parent"
                        , Just "someNameCategory>parentCategory")] 
      `shouldBe` Query "406cn"
  it "Method is change_parent: this parent category doesn't exist" $                               
    editCategory' True [("change_parent",Just "existCategory>someNameCategory")] 
      `shouldBe` Query "406cp"            
  it "Method is change_parent: everything is all right"$                               
    editCategory' True [("change_parent",Just "existCategory>parentCategory")] 
      `shouldBe` Query "UPDATE category \
                       \ SET parent_category = 'parentCategory' \
                       \ WHERE name_category = 'existCategory'; "                
  it "Method is change_parent: two elements in list" $                               
    editCategory' True [ ("change_parent",Just "existCategory>parentCategory")
                       , ("change_parent",Just "parentCategory>Null") 
                       ] 
      `shouldBe` Query "UPDATE category \
                       \ SET parent_category = 'parentCategory' \
                       \ WHERE name_category = 'existCategory'; "       
  it "Method is change_parent: with 'Nothing'"$                               
    editCategory' True [("change_parent",Nothing)] 
      `shouldBe` Query "404"            
  it "Method is change_parent: this category doesn't exist" $                               
    editCategory' True [("change_parent",Just "newCategory>parentCategory")] 
      `shouldBe` Query "406cn"  
     
