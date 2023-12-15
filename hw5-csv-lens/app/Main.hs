{-# LANGUAGE OverloadedStrings #-}

module Main where

import CSVParser
import Songs
import ParsingDataRows
import SongsFunctions

import Text.Megaparsec (errorBundlePretty)

-- main :: IO ()
-- main = do
--   let exampleCSV = "track_id,track_name,track_artist,track_popularity,track_album_id,track_album_name,track_album_release_date,playlist_name,playlist_id,playlist_genre,playlist_subgenre,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms\n1,Some Cool Track 1,Some Artist,800,101,Album 1,2022-01-01,My Playlist,201,Dance,Electronic,0.7,0.8,4,-5.0,6,0.6,0.2,0.6,0.4,0.5,120,300000\ninvalid_row\n2,Another Track,Another Artist,70,102,Album 2,2022-02-01,Your Playlist,202,Pop,Rock,0.6,0.7,7,-6.0,0,0.2,0.3,0.4,0.5,0.6,130,240000\n"
--       delimiter = ','
--       settings = CSVParserSettings delimiter True
--   case parseCSV delimiter settings exampleCSV of
--     Left err -> putStrLn $ "Error parsing CSV:\n" ++ errorBundlePretty err
--     Right csv -> do
--       -- putStrLn "Parsed CSV:"
--       -- print csv
--       putStrLn "Data Rows:"
--       let dataRows = filterValidDataRows (map dataRowParser (rows csv))
--       mapM_ print dataRows


-- main :: IO ()
-- main = do
--   let albums = [Album "11" "Album 1" "2023-09-11" [Track "1" "Track 1" 300000, Track "2" "Track 2" 240000]
--                    , Album "12" "Album 2" "2023-12-01" [Track "3" "Track 3" 180000]]
          
--   case findLongestAlbum albums of
--     Just longestAlbum -> do
--       putStrLn "!! Found the longest album: "
--       print longestAlbum
--     Nothing -> putStrLn "No albums found"


-- main :: IO ()
-- main = do
--   let albums = [
--         Album "1" "Album 1" "2023-09-11" [Track "1" "Track 1" 300000, Track "2" "Track 2" 240000]
--         , Album "1" "Album 2" "2023-12-01" [Track "3" "Track 3" 180000]
--         , Album "1" "Album 3" "2024-12-01" [Track "4" "Track 4" 18000]
--         ]

--   case findAlbumWithLongestAverageDuration albums of
--     Just album -> do
--       putStrLn "Result of findAlbumWithLongestAverageDuration: "
--       print album
--     Nothing -> putStrLn "No albums found"

-- main :: IO ()
-- main = do
--   let albums = [
--         Album "11" "Album 1" "2023-09-11" [Track "1" "Track 1" 300000, Track "2" "Track 2" 240000]
--         , Album "12" "Album 2" "2023-12-01" [Track "3" "Track 3" 180000]
--         , Album "13" "Album 3" "2024-12-01" [Track "4" "Track 4" 18000]
--         ]

--   case findAlbumWithShortestTrack albums of
--     Just album -> do
--       putStrLn "Result of findAlbumWithShortestTrack: "
--       print album
--     Nothing -> putStrLn "No albums found"



-- main :: IO ()
-- main = do
--   let albums = [Album "1" "Album 1" "2023-01-01" []
--                , Album "2" "Album 2" "2024-02-01" []
--                , Album "3" "Album 3" "2022-12-31" []]

--       targetYear = 2023

--       albumsByYear = findAlbumsByYear targetYear albums

--   print albumsByYear