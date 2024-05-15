module PubDomain where


data GeoLocation = GeoLocation
  {
    lat      :: Double
  , lon      :: Double
  } deriving (Show, Eq)

data Pub = Pub
  {  
    uid       :: Int
  , name     :: String
  , geoLoc   :: GeoLocation
  } deriving (Show, Eq)
