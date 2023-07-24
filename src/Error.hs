module Error where

data Error = CommonError | LoginOccupied | CategoryExists | NoCategory | NoParentCategory | CategoryParentItself
  deriving (Show, Eq)
