module Lib where

import Data.String (IsString)
import qualified Data.Text as T
import Text.Read (readMaybe)

readNum :: T.Text -> Int
readNum n =
  case readMaybe $ T.unpack n of
    Just x -> x
    _ -> 0

initTxt :: T.Text -> T.Text
initTxt "" = ""
initTxt txt = T.init txt

tailTxt :: T.Text -> T.Text
tailTxt "" = ""
tailTxt txt = T.tail txt

splitOnTxt :: T.Text -> T.Text -> [T.Text]
splitOnTxt _ "" = []
splitOnTxt c txt = T.splitOn c txt

-- Pulls a value from a list of lists.
drawOut :: Data.String.IsString a => [[a]] -> a
drawOut [] = ""
drawOut ([] : _) = ""
drawOut ([x] : _) = x
drawOut (_ : _) = ""
