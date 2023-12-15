{-# LANGUAGE OverloadedStrings #-}

module Main where

import CSVParser

import Text.Megaparsec
import Data.Either (rights)
import Text.Megaparsec.Char
import Control.Monad (void)
import Data.Void

data DataRow = DataRow
  { trackId :: String
  , trackName :: String
  , trackAlbumId :: String
  , trackAlbumName :: String
  , trackAlbumReleaseDate :: String
  , trackDuration :: Int
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

main :: IO ()
main = do
  let exampleCSV = "track_id,track_name,track_artist,track_popularity,track_album_id,track_album_name,track_album_release_date,playlist_name,playlist_id,playlist_genre,playlist_subgenre,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms\n1,Some Cool Track 1,Some Artist,800,101,Album 1,2022-01-01,My Playlist,201,Dance,Electronic,0.7,0.8,4,-5.0,6,0.6,0.2,0.6,0.4,0.5,120,300000\ninvalid_row\n2,Another Track,Another Artist,70,102,Album 2,2022-02-01,Your Playlist,202,Pop,Rock,0.6,0.7,7,-6.0,0,0.2,0.3,0.4,0.5,0.6,130,240000\n"
      delimiter = ','
      settings = CSVParserSettings delimiter True
  case parseCSV delimiter settings exampleCSV of
    Left err -> putStrLn $ "Error parsing CSV:\n" ++ errorBundlePretty err
    Right csv -> do
      -- putStrLn "Parsed CSV:"
      -- print csv
      putStrLn "Data Rows:"
      let dataRows = filterValidDataRows (map dataRowParser (rows csv))
      mapM_ print dataRows
