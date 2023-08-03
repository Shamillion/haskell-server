module Error where

data Error
  = CommonError
  | LoginOccupied
  | CategoryError CategoryError
  | ParseError ParseError
  deriving (Show, Eq)

data CategoryError
  = CategoryExists
  | NoCategory
  | NoParentCategory
  | CategoryParentItself
  deriving (Show, Eq)

data ParseError
  = DecodeImageError
  | ParseCategoryError
  | ParseNewsError
  | ParseUserError
  deriving (Show, Eq)
