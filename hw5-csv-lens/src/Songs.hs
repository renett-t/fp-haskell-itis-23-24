{-# LANGUAGE TemplateHaskell #-}

module Songs where


data Track = Track
  { _trackId :: String
  , _name :: String
  , _duration :: Int
  }
  deriving (Show, Eq)


data Album = Album
  { _albumId :: String
  , _albumName :: String
  , _albumReleaseDate :: String
  , _albumTracks :: [Track]
  }
  deriving (Show, Eq)
