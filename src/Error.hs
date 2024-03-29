module Error where

import Config (Priority (ERROR, INFO))
import Control.Exception (Exception, throwIO)
import Control.Monad.Reader (liftIO)
import Environment (Flow)
import Logger (writingLine)

data Error
  = CommonError
  | DatabaseError
  | LoginOccupied
  | AuthError AuthError
  | CategoryError CategoryError
  | ParseError ParseError
  | NewsError NewsError
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
  | InvalidPassword
  deriving (Show, Eq)

instance Exception AuthError

data NewsError
  = UserNotAuthor
  | NotAuthorThisNews
  deriving (Show, Eq)

instance Exception NewsError

throwError :: Error -> Flow a
throwError err = do
  let priority = if err `elem` errorLs then ERROR else INFO
      errorLs = [AuthError DecodeLoginAndPassError, ParseError DecodeImageError, DatabaseError]
  writingLine priority $ show err
  liftIO $ throwIO err
