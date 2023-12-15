{-# LANGUAGE TemplateHaskell #-}

module ParsingDataRows where

import Songs

import Data.Either (rights)
import Control.Lens
import Data.List (groupBy)
import Data.Function (on)


data DataRow = DataRow
  { _dataRowTrackId :: String
  , _trackName :: String
  , _trackAlbumId :: String
  , _trackAlbumName :: String
  , _trackAlbumReleaseDate :: String
  , _trackDuration :: Int
  }
  deriving (Show, Eq)


dataRowParser :: [String] -> Either String DataRow
dataRowParser columns =
  case columns of
    [trackId, trackName, _, _, trackAlbumId, trackAlbumName, trackAlbumReleaseDate, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, duration] ->
      Right $ DataRow trackId trackName trackAlbumId trackAlbumName trackAlbumReleaseDate (read duration :: Int)
    _ -> Left "Invalid number of columns"

-- Пытаемся превратить наш CSV в список DataRow
csvRowsToDataRows :: [[String]] -> [Either String DataRow]
csvRowsToDataRows rows = map dataRowParser rows

filterValidDataRows :: [Either String DataRow] -> [DataRow]
filterValidDataRows = rights



--- линзыыы..... whyyyy _

makeLenses ''DataRow
makeLenses ''Track
makeLenses ''Album


dataRowsToAlbums :: [DataRow] -> [Album]
dataRowsToAlbums dataRows =
  let groupedDataRows = groupByAlbumId dataRows
  in fmap (\group -> Album
                   { _albumId = view trackAlbumId (head group)
                   , _albumName = view trackAlbumName (head group)
                   , _albumReleaseDate = view trackAlbumReleaseDate (head group)
                   , _albumTracks = fmap (\row -> Track
                                                { _trackId = view dataRowTrackId row
                                                , _name = view trackName row
                                                , _duration = view trackDuration row
                                                }) group
                   }) groupedDataRows

groupByAlbumId :: [DataRow] -> [[DataRow]]
groupByAlbumId = groupBy ((==) `on` view trackAlbumId)
