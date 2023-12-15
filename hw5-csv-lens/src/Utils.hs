module Utils where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- Парсит год из строки с датой формата 2019-12-13
parseYear :: String -> Maybe Int
parseYear dateString =
  case splitOn "-" dateString of
    [year, _, _] -> readMaybe year
    _            -> Nothing
