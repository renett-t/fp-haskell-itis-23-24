{-# LANGUAGE TemplateHaskell #-}

module SongsFunctions where

import Songs
import Utils

import Control.Lens
import Data.Ord
import Data.List (sortBy, maximumBy, minimumBy)


data AlbumDurationInfo = AlbumDurationInfo
  { albumRef :: Album
  , totalAlbumDuration :: Int
  }
  deriving (Eq, Show)

makeLenses ''Track
makeLenses ''Album


-- функция поиска самого длинного альбома (по сумме длин песен);
findLongestAlbum :: [Album] -> Maybe AlbumDurationInfo
findLongestAlbum albums =
  case sortedAlbums of
    [] -> Nothing
    (info : _) -> Just info
  where
    albumInfos = fmap (\album -> AlbumDurationInfo album (sumOf (albumTracks.traverse.duration) album)) albums
    -- Сортировка по убыванию длительности
    sortedAlbums = sortBy (comparing (Down . totalAlbumDuration)) albumInfos


-- функция поиска альбома с наибольшей средней длиной песни;
averageDurationHelper :: Album -> Double
averageDurationHelper album =
  let trackDurations = map (fromIntegral . _duration) (_albumTracks album)
      totalDuration = sum trackDurations
      numTracks = length trackDurations
  in if numTracks == 0
       then 0.0
       else totalDuration / fromIntegral numTracks

findAlbumWithLongestAverageDuration :: [Album] -> Maybe Album
findAlbumWithLongestAverageDuration albums =
  case albums of
    [] -> Nothing
    _ ->
      let maxAlbum = maximumBy (comparing averageDurationHelper) albums
      in Just maxAlbum


-- функция поиска самого короткого альбома;
findAlbumWithShortestTrack :: [Album] -> Maybe Album
findAlbumWithShortestTrack [] = Nothing
findAlbumWithShortestTrack albums =
  Just $ minimumBy (comparing (minimum . map _duration . _albumTracks)) albums


-- функция поиска альбомов по году
findAlbumsByYear :: Int -> [Album] -> [Album]
findAlbumsByYear targetYear = filter (\album -> maybe False (== targetYear) (parseYear (_albumReleaseDate album)))
