module Main where

import PubService
import PubDomain

import Text.Printf (printf)


isPubInIds :: [Int] -> Pub -> Bool
isPubInIds pubIds pub = uid pub `elem` pubIds

filterPubsByIds :: [Int] -> [Pub] -> [Pub]
filterPubsByIds pubIds pubs = filter (isPubInIds pubIds) pubs

printNearestPubs :: (Pub, [Pub]) -> IO ()
printNearestPubs (pub, nearbyPubs) = do
  putStrLn $ printf "%s has %d nearby pubs:" (show pub) (length nearbyPubs)
  mapM_ (\p -> putStrLn $ "  " ++ show p) nearbyPubs


main :: IO ()
main = do
  pubs <- readData

  let pubIds = [54, 56, 57] :: [Int]
  let filteredPubs = filterPubsByIds pubIds pubs

  let nearestPubsResult = nearest filteredPubs

  putStrLn "\nNearest pubs for specified IDs: "
  mapM_ printNearestPubs nearestPubsResult
