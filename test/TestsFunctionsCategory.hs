module TestsFunctionsCategory
  ( testsFunctionCreateCategory,
    testsFunctionEditCategory,
  )
where

import Category (CategoryHandle (..), createCategory, editCategory)
import qualified Data.ByteString.Char8 as BC
import Data.Functor.Identity (Identity, runIdentity)
import Data.String (IsString)
import Database.PostgreSQL.Simple.Types (Query (..))
import Error (CategoryError (..), Error (..))
import Test.Hspec (SpecWith, it, shouldBe)

categoryTestHandler :: CategoryHandle () Identity
categoryTestHandler = CategoryHandle {checkUniqCategoryH = testUniqCategory}

testUniqCategory :: (Eq a, Data.String.IsString a) => () -> a -> Identity Bool
testUniqCategory _ val = pure $ val `notElem` ["parentCategory", "Null", "existCategory"]

createCategory' :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Either Error Query
createCategory' bool ls = runIdentity $ createCategory categoryTestHandler () bool ls

testsFunctionCreateCategory :: SpecWith ()
testsFunctionCreateCategory = do
  it "User is not admin" $
    createCategory' False [("Cars>Wheels", Nothing)] `shouldBe` Left CommonError
  it "User is not admin and list is empty" $
    createCategory' False [] `shouldBe` Left CommonError
  it "User is admin and list is empty" $
    createCategory' True [] `shouldBe` Left CommonError
  it "Everything is all right" $
    createCategory' True [("parentCategory>category", Nothing)]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'parentCategory');"
        )
  it "'Just' instead 'Nothing'" $
    createCategory' True [("parentCategory>category", Just "anything")]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'parentCategory');"
        )
  it "Parent category does not exist" $
    createCategory' True [("notExistCategory>category", Nothing)]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Name of category is not unique" $
    createCategory' True [("parentCategory>existCategory", Nothing)]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Parent category does not exist and name of category is not unique" $
    createCategory' True [("notExistCategory>existCategory", Nothing)]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Name of category and name of parent category are equal" $
    createCategory' True [("parentCategory>parentCategory", Nothing)]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Name of category and name of parent category are equal and don't exist" $
    createCategory' True [("notExistCategory>notExistCategory", Nothing)]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Syntax mistake in request" $
    createCategory' True [("parentCategory<category", Nothing)]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('parentCategory<category', 'Null');"
        )
  it "Syntax mistake in request - 2" $
    createCategory' True [("parentCategory>>category", Nothing)]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'parentCategory');"
        )
  it "Categories are more than 2" $
    createCategory' True [("parentCategory>category_1>category_2", Nothing)]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category_1', 'parentCategory');"
        )
  it "2 elements into list" $
    createCategory'
      True
      [ ("parentCategory>category_1", Nothing),
        ("parentCategory>category_2", Nothing)
      ]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category_1', 'parentCategory');"
        )
  it "3 elements into list" $
    createCategory'
      True
      [ ("parentCategory>category_1", Nothing),
        ("parentCategory>category_2", Nothing),
        ("parentCategory>category_3", Nothing)
      ]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category_1', 'parentCategory');"
        )
  it "Without parent category" $
    createCategory' True [(">category", Nothing)]
      `shouldBe` Right
        ( Query
            "INSERT INTO category (name_category, parent_category) \
            \ VALUES ('category', 'Null');"
        )
  it "Without category" $
    createCategory' True [("parentCategory>", Nothing)]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Without both categories" $
    createCategory' True [(">", Nothing)]
      `shouldBe` Left CommonError

editCategory' :: Bool -> [(BC.ByteString, Maybe BC.ByteString)] -> Either Error Query
editCategory' bool ls = runIdentity $ editCategory categoryTestHandler () bool ls

testsFunctionEditCategory :: SpecWith ()
testsFunctionEditCategory = do
  -- [("change_name",Just "aaa>bbb")]
  it "User is not admin" $
    editCategory' False [("change_name", Just "existCategory>newCategory")]
      `shouldBe` Left CommonError
  it "User is not admin and list is empty" $
    editCategory' False [] `shouldBe` Left CommonError
  it "User is admin and list is empty" $
    editCategory' True [] `shouldBe` Left CommonError
  it "Unknown method" $
    editCategory' True [("changeName", Just "existCategory>newCategory")]
      `shouldBe` Left CommonError
  it "Without old name" $
    editCategory' True [("change_name", Just ">newCategory")]
      `shouldBe` Left CommonError
  it "Without new name" $
    editCategory' True [("change_name", Just "existCategory>")]
      `shouldBe` Left CommonError
  it "Without both names" $
    editCategory' True [("change_name", Just ">")]
      `shouldBe` Left CommonError
  it "Syntax error" $
    editCategory' True [("change_name", Just "existeCategorynewCategory")]
      `shouldBe` Left CommonError
  it "Syntax error 2" $
    editCategory' True [("change_name", Just "existCategory>>newCategory")]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET   parent_category = 'newCategory' \
            \ WHERE parent_category = 'existCategory'; \
            \ UPDATE category \
            \ SET   name_category = 'newCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it "With 3 category's name" $
    editCategory'
      True
      [ ( "change_name",
          Just "existCategory>newCategory>newCategory_2"
        )
      ]
      `shouldBe` Left CommonError
  it "With Nothing" $
    editCategory' True [("change_name", Nothing)]
      `shouldBe` Left CommonError
  it "Two elements in list. One element with syntax error" $
    editCategory'
      True
      [ ("change_name", Just "existCategory>newCategory"),
        ("change_name", Just "existCategory>>newCategory_2")
      ]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET   parent_category = 'newCategory' \
            \ WHERE parent_category = 'existCategory'; \
            \ UPDATE category \
            \ SET   name_category = 'newCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it "Two elements in list with Nothing" $
    editCategory'
      True
      [ ("change_name", Just "existCategory>newCategory"),
        ("change_name", Nothing)
      ]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET   parent_category = 'newCategory' \
            \ WHERE parent_category = 'existCategory'; \
            \ UPDATE category \
            \ SET   name_category = 'newCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it "Method is change_name: such category already exists" $
    editCategory' True [("change_name", Just "parentCategory>existCategory")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Method is change_name: new name and old name are the same" $
    editCategory' True [("change_name", Just "existCategory>existCategory")]
      `shouldBe` Left (CategoryError CategoryExists)
  it "Method is change_name: this category doesn't exist" $
    editCategory' True [("change_name", Just "someNameCategory>newCategory")]
      `shouldBe` Left (CategoryError NoCategory)
  it "Method is change_name: everything is all right" $
    editCategory' True [("change_name", Just "existCategory>newCategory")]
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
    "Method is change_parent: names of category and parent \
    \category are the same"
    $ editCategory' True [("change_parent", Just "existCategory>existCategory")]
      `shouldBe` Left (CategoryError CategoryParentItself)
  it "Method is change_parent: this category doesn't exist" $
    editCategory'
      True
      [ ( "change_parent",
          Just "someNameCategory>parentCategory"
        )
      ]
      `shouldBe` Left (CategoryError NoCategory)
  it "Method is change_parent: this parent category doesn't exist" $
    editCategory' True [("change_parent", Just "existCategory>someNameCategory")]
      `shouldBe` Left (CategoryError NoParentCategory)
  it "Method is change_parent: everything is all right" $
    editCategory' True [("change_parent", Just "existCategory>parentCategory")]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET parent_category = 'parentCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it "Method is change_parent: two elements in list" $
    editCategory'
      True
      [ ("change_parent", Just "existCategory>parentCategory"),
        ("change_parent", Just "parentCategory>Null")
      ]
      `shouldBe` Right
        ( Query
            "UPDATE category \
            \ SET parent_category = 'parentCategory' \
            \ WHERE name_category = 'existCategory'; "
        )
  it "Method is change_parent: with 'Nothing'" $
    editCategory' True [("change_parent", Nothing)]
      `shouldBe` Left CommonError
  it "Method is change_parent: this category doesn't exist" $
    editCategory' True [("change_parent", Just "newCategory>parentCategory")]
      `shouldBe` Left (CategoryError NoCategory)
