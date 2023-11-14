module TestsFunctionsCategory
  ( testsFunctionCreateCategory,
    testsFunctionEditCategory,
  )
where

import qualified Data.ByteString.Char8 as BC
import Data.Functor.Identity (Identity, runIdentity)
import Data.String (IsString)
import Database.PostgreSQL.Simple.Types (Query (..))
import Endpoints.Category (CategoryHandle (..), createCategory, editCategory)
import Error (CategoryError (..), Error (..))
import Test.Hspec (SpecWith, it, shouldBe)

categoryTestHandler :: CategoryHandle Identity
categoryTestHandler = CategoryHandle {checkUniqCategoryH = testUniqCategory}

testUniqCategory :: (Eq a, Data.String.IsString a) => a -> Identity Bool
testUniqCategory val = pure $ val `notElem` ["parentCategory", "Null", "existCategory", "existCategory2"]

createCategory' :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Either Error Query
createCategory' bool ls = runIdentity $ createCategory categoryTestHandler bool ls

testsFunctionCreateCategory :: SpecWith ()
testsFunctionCreateCategory = do
  it "User is not admin" $
    createCategory' False [("categoryName", Just "Wheels"), ("parentName", Just "Cars")] `shouldBe` Left CommonError
  it "User is not admin and list is empty" $
    createCategory' False [] `shouldBe` Left CommonError
  it "User is admin and list is empty" $
    createCategory' True [] `shouldBe` Left CommonError
  it "Everything is all right" $
    createCategory' True [("categoryName", Just "category"), ("parentName", Just "parentCategory")]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'parentCategory');"
        )
  it "'Nothing' in the first parameter" $
    createCategory' True [("categoryName", Nothing), ("parentName", Just "parentCategory")]
      `shouldBe` Left CommonError
  it "'Nothing' in the second parameter" $
    createCategory' True [("categoryName", Just "category"), ("parentName", Nothing)]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'Null');"
        )
  it "'Nothing' in both parameters" $
    createCategory' True [("categoryName", Nothing), ("parentName", Nothing)]
      `shouldBe` Left CommonError
  it "Empty string instead of the category name" $
    createCategory' True [("categoryName", Just ""), ("parentName", Just "parentCategory")]
      `shouldBe` Left CommonError
  it "Empty string instead of the parent category name" $
    createCategory' True [("categoryName", Just "category"), ("parentName", Just "")]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Empty string instead of both parameters" $
    createCategory' True [("categoryName", Just ""), ("parentName", Just "parentCategory")]
      `shouldBe` Left CommonError
  it "Parent category does not exist" $
    createCategory' True [("categoryName", Just "category"), ("parentName", Just "notExistCategory")]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Name of category is not unique" $
    createCategory' True [("categoryName", Just "existCategory"), ("parentName", Just "parentCategory")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Parent category does not exist and name of category is not unique" $
    createCategory' True [("categoryName", Just "existCategory"), ("parentName", Just "notExistCategory")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Name of category and name of parent category are equal" $
    createCategory' True [("categoryName", Just "parentCategory"), ("parentName", Just "parentCategory")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Name of category and name of parent category are equal and don't exist" $
    createCategory' True [("categoryName", Just "notExistCategory"), ("parentName", Just "notExistCategory")]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Syntax mistake in request" $
    createCategory' True [("categoryName", Just "categoryparentName=parentCategory")]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('categoryparentName=parentCategory', 'Null');"
        )
  it "Categories are more than 2" $
    createCategory'
      True
      [ ("categoryName", Just "category_1"),
        ("categoryName", Just "category_1"),
        ("parentName", Just "parentCategory")
      ]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category_1', 'parentCategory');"
        )
  it "Without parent category" $
    createCategory' True [("categoryName", Just "category")]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'Null');"
        )
  it "Without category" $
    createCategory' True [("parentName", Just "parentCategory")]
      `shouldBe` Left CommonError

editCategory' :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Either Error Query
editCategory' bool ls = runIdentity $ editCategory categoryTestHandler bool ls

testsFunctionEditCategory :: SpecWith ()
testsFunctionEditCategory = do
  it "User is not admin" $
    editCategory' False [("categoryName", Just "existCategory"), ("newCategoryName", Just "newCategory")]
      `shouldBe` Left CommonError
  it "User is not admin and list is empty" $
    editCategory' False [] `shouldBe` Left CommonError
  it "User is admin and list is empty" $
    editCategory' True [] `shouldBe` Left CommonError
  it "Unknown method" $
    editCategory' True [("newCategoryName", Just "newCategory"), ("parentName", Just "existCategory")]
      `shouldBe` Left CommonError
  it "Another unknown method" $
    editCategory' True [("categoryName", Just "existCategory"), ("newName", Just "newCategory")]
      `shouldBe` Left CommonError
  it "Without old name" $
    editCategory' True [("newCategoryName", Just "newCategory")]
      `shouldBe` Left CommonError
  it "Without new name" $
    editCategory' True [("categoryName", Just "existCategory")]
      `shouldBe` Left CommonError
  it "Empty string instead of the category name" $
    editCategory' True [("categoryName", Just ""), ("newCategoryName", Just "newCategory")]
      `shouldBe` Left CommonError
  it "Empty string instead of the new name of category" $
    editCategory' True [("categoryName", Just "existCategory"), ("newCategoryName", Just "")]
      `shouldBe` Left CommonError
  it "Empty string instead of the name of the parent category" $
    editCategory' True [("categoryName", Just "existCategory"), ("parentName", Just "")]
      `shouldBe` Left CommonError
  it "Empty strings instead of the name of the names of both categories" $
    editCategory' True [("categoryName", Just ""), ("parentName", Just "")]
      `shouldBe` Left CommonError
  it "'Nothing' instead of the category name" $
    editCategory' True [("categoryName", Nothing), ("newCategoryName", Just "newCategory")]
      `shouldBe` Left CommonError
  it "'Nothing' instead of the new name of category" $
    editCategory' True [("categoryName", Just "existCategory"), ("newCategoryName", Nothing)]
      `shouldBe` Left CommonError
  it "'Nothing' instead of the name of the parent category" $
    editCategory' True [("categoryName", Just "existCategory"), ("parentName", Nothing)]
      `shouldBe` Left CommonError
  it "'Nothing' instead of the names of both categories" $
    editCategory' True [("categoryName", Nothing), ("parentName", Nothing)]
      `shouldBe` Left CommonError
  it "With 3 category's name" $
    editCategory'
      True
      [ ("categoryName", Just "existCategory"),
        ("categoryName", Just "existCategory2"),
        ("newCategoryName", Just "newCategory")
      ]
      `shouldBe` Left CommonError
  it "Method is ChangeName: such category already exists" $
    editCategory' True [("categoryName", Just "existCategory"), ("newCategoryName", Just "existCategory2")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Method is ChangeName: new name and old name are the same" $
    editCategory' True [("categoryName", Just "existCategory"), ("newCategoryName", Just "existCategory")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Method is ChangeName: this category doesn't exist" $
    editCategory' True [("categoryName", Just "someNameCategory"), ("newCategoryName", Just "newCategory")]
      `shouldBe` Left (CategoryError NoCategory)
  it "Method is ChangeName: everything is all right" $
    editCategory' True [("categoryName", Just "existCategory"), ("newCategoryName", Just "newCategory")]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET   parent_category = 'newCategory' \
            \ WHERE parent_category = 'existCategory'; \
            \ UPDATE category \
            \ SET   name_category = 'newCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it
    "Method is ChangeParent: names of category and parent \
    \category are the same"
    $ editCategory' True [("categoryName", Just "existCategory"), ("parentName", Just "existCategory")]
      `shouldBe` Left (CategoryError CategoryParentItself)
  it "Method is ChangeParent: this category doesn't exist" $
    editCategory'
      True
      [("categoryName", Just "someNameCategory"), ("parentName", Just "parentCategory")]
      `shouldBe` Left (CategoryError NoCategory)
  it "Method is ChangeParent: this parent category doesn't exist" $
    editCategory' True [("categoryName", Just "existCategory"), ("parentName", Just "someNameCategory")]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Method is ChangeParent: everything is all right" $
    editCategory' True [("categoryName", Just "existCategory"), ("parentName", Just "parentCategory")]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET parent_category = 'parentCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it "Method is ChangeParent: three elements in list" $
    editCategory'
      True
      [ ("categoryName", Just "existCategory"),
        ("categoryName", Just "existCategory2"),
        ("parentName", Just "parentCategory")
      ]
      `shouldBe` Left CommonError
  it "Method is ChangeParent: this category doesn't exist" $
    editCategory' True [("categoryName", Just "newCategory"), ("parentName", Just "parentCategory")]
      `shouldBe` Left (CategoryError NoCategory)
