module Error where

import Control.Exception (Exception)

data Error
  = CommonError
  | LoginOccupied
  | CategoryError CategoryError
  | ParseError ParseError
  deriving (Show, Eq)

instance Exception Error

data CategoryError
  = CategoryExists
  | NoCategory
  | NoParentCategory
  | CategoryParentItself
  deriving (Show, Eq)

instance Exception CategoryError

data ParseError
  = DecodeImageError
  | ParseCategoryError
  | ParseNewsError
  | ParseUserError
  deriving (Show, Eq)

instance Exception ParseError

data AuthError
  = NoAuthorization
  | DecodeLoginAndPassError
  | NoSuchUserInDB
  deriving (Show, Eq)

instance Exception AuthError
