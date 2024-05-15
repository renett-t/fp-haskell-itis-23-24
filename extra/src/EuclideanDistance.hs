module EuclideanDistance where

import PubDomain 


euclideanDistance :: GeoLocation -> GeoLocation -> Double
euclideanDistance (GeoLocation lat1 lon1) (GeoLocation lat2 lon2) =
  sqrt ((lat1 - lat2) ** 2 + (lon1 - lon2) ** 2)
