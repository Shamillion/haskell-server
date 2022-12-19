{-# LANGUAGE OverloadedStrings #-}

module TestsFunctionsCategory 
  ( testsFunctionCreateCategoryWith
  , testsFunctionEditCategoryWith
  ) 
where

import Test.Hspec
import Test.QuickCheck
import Database.PostgreSQL.Simple.Types
import Category (createCategoryWith, editCategoryWith)




testUniqCategory e = notElem e ["parentCategory", "Null", "existeCategory"]

createCategory' = createCategoryWith testUniqCategory

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
    createCategory' True [("parentCategory>existeCategory",Nothing)] 
      `shouldBe` Query "406cu"
  it "Parent category does not exist and name of category is not unique" $                               
    createCategory' True [("notExistCategory>existeCategory",Nothing)] 
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

editCategory' = editCategoryWith testUniqCategory

testsFunctionEditCategoryWith = do                            -- [("change_name",Just "aaa>bbb")]
  it "User is not admin" $                               
    editCategory' False [("change_name",Just "existeCategory>newCategory")] 
      `shouldBe` Query "404" 
  it "User is not admin and list is empty" $                               
    editCategory' False [] `shouldBe` Query "404"     
  it "User is admin and list is empty" $                               
    editCategory' True [] `shouldBe` Query "404" 
  it "Unknown method" $                               
    editCategory' True [("changeName",Just "existeCategory>newCategory")] 
      `shouldBe` Query "404"     
  it "Without old name" $                               
    editCategory' True [("change_name",Just ">newCategory")] 
      `shouldBe` Query "404"
  it "Without new name" $                               
    editCategory' True [("change_name",Just "existeCategory>")] 
      `shouldBe` Query "404"       
  it "Without both names" $                               
    editCategory' True [("change_name",Just ">")] 
      `shouldBe` Query "404"
  it "Syntax error" $                               
    editCategory' True [("change_name",Just "existeCategorynewCategory")] 
      `shouldBe` Query "404"      
  it "Syntax error 2" $                               
    editCategory' True [("change_name",Just "existeCategory>>newCategory")] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existeCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existeCategory'; "    
  it "With 3 category's name" $                               
    editCategory' True [( "change_name"
                        , Just "existeCategory>newCategory>newCategory_2"
                        )] 
      `shouldBe` Query "404"                  
  it "With Nothing" $                               
    editCategory' True [("change_name",Nothing)] 
      `shouldBe` Query "404"
  it "Two elements in list" $                               
    editCategory' True [ ("change_name",Just "existeCategory>newCategory")
                       , ("change_name",Just "existeCategory>newCategory_2") 
                       ] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existeCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existeCategory'; \
                       \UPDATE category \
                       \ SET   parent_category = 'newCategory_2' \
                       \ WHERE parent_category = 'existeCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory_2' \
                       \ WHERE name_category = 'existeCategory'; "             
  it "Two elements in list and Nothing" $                               
    editCategory' True [ ("change_name",Just "existeCategory>newCategory")
                       , ("change_name",Just "existeCategory>newCategory_2") 
                       , ("change_name",Nothing)
                       ] 
      `shouldBe` Query "UPDATE category \
                       \ SET   parent_category = 'newCategory' \
                       \ WHERE parent_category = 'existeCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory' \
                       \ WHERE name_category = 'existeCategory'; \
                       \UPDATE category \
                       \ SET   parent_category = 'newCategory_2' \
                       \ WHERE parent_category = 'existeCategory'; \
                       \ UPDATE category \
                       \ SET   name_category = 'newCategory_2' \
                       \ WHERE name_category = 'existeCategory'; "      
  it "Three elements in list. One element with error in method." $                               
    editCategory' True [ ("change_name",Just "existeCategory>newCategory")
                       , ("change_name",Just "existeCategory>newCategory_2") 
                       , ("changeName",Just "existeCategory>newCategory_3")
                       ] 
      `shouldBe` Query "404"     
  it "Three elements in list. One element with error in category's names." $                               
    editCategory' True [ ("change_name",Just "existeCategory>newCategory")
                       , ("change_name",Just "existeCategory>newCategory_2") 
                       , ("change_name",Just "existeCategorynewCategory_3")
                       ] 
      `shouldBe` Query "404"      
      
