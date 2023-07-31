module Error where

data Error = CommonError | LoginOccupied | CategoryExists | NoCategory | NoParentCategory | CategoryParentItself
  deriving (Show, Eq)

data ParseError = DecodeImageError | ParseCategoryError | ParseNewsError | ParseUserError | AnotherError
  deriving (Show, Eq)
