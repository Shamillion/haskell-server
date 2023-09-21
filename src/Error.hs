module Error where

import Config (Priority (DEBUG, ERROR))
import Control.Exception (Exception, throwIO)
import Control.Monad.Reader (liftIO)
import Environment (Flow)
import Logger (writingLine)

data Error
  = CommonError
  | LoginOccupied
  | AuthError AuthError
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

throwError :: Error -> Flow a
throwError err = do
  let priority = if err `elem` errorLs then ERROR else DEBUG
      errorLs = [AuthError DecodeLoginAndPassError, ParseError DecodeImageError]
  writingLine priority $ show err
  liftIO $ throwIO err
