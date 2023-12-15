{-# LANGUAGE TemplateHaskell #-}

module SongsFunctions where

import Songs

import Control.Lens
import Data.Ord
import Data.List (sortBy)


data AlbumDurationInfo = AlbumDurationInfo
  { albumRef :: Album
  , totalAlbumDuration :: Int
  }
  deriving (Eq, Show)

makeLenses ''Track
makeLenses ''Album

findLongestAlbum :: [Album] -> Maybe AlbumDurationInfo
findLongestAlbum albums =
  case sortedAlbums of
    [] -> Nothing
    (info : _) -> Just info
  where
    albumInfos = fmap (\album -> AlbumDurationInfo album (sumOf (albumTracks.traverse.duration) album)) albums
    -- Сортировка по убыванию длительности
    sortedAlbums = sortBy (comparing (Down . totalAlbumDuration)) albumInfos
