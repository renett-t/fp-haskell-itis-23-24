module RowsConversions where

import PubDomain

import Data.Either (rights)
import Control.Arrow (left)


data Row = Row
  { _uid       :: Int,
    fsa      :: String,
    _name     :: String,
    address  :: String,
    postcode :: String,
    easting  :: Double,
    northing :: Double,
    _lat      :: Double,
    _lon      :: Double,
    town     :: String
  } deriving (Show, Eq)

-- Converting a list of strings from CSV row to a Row instance
parseRow :: [String] -> Either String Row
parseRow [idStr, fsa, name, address, postcode, eastingStr, northingStr, latStr, lonStr, town] =
  Row <$> parseInt idStr <*> pure fsa <*> pure name <*> pure address <*> pure postcode
      <*> parseDouble eastingStr <*> parseDouble northingStr <*> parseDouble latStr
      <*> parseDouble lonStr <*> pure town
parseRow _ = Left "Invalid input: Row does not contain all required fields"

parseInt :: String -> Either String Int
parseInt str = case reads str of
  [(x, "")] -> Right x
  _         -> Left $ "Failed to parse Int: " ++ str

parseDouble :: String -> Either String Double
parseDouble str = case reads str of
  [(x, "")] -> Right x
  _         -> Left $ "Failed to parse Double: " ++ str


-- Converting a Row instance to a Pub instance
convertRowToPub :: Row -> Pub
convertRowToPub row = Pub
  { uid     = _uid row
  , name   = _name row
  , geoLoc = GeoLocation (_lat row) (_lon row)
  }


-- Converting CSV into [Row]
csvRowsToDataRows :: [[String]] -> [Either String Row]
csvRowsToDataRows rows = map parseRow rows

filterValidDataRows :: [Either String Row] -> [Row]
filterValidDataRows = rights
