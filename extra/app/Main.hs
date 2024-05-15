module Main where

import PubService
import PubDomain

import Text.Printf (printf)


printNearestPubs :: (Pub, [Pub]) -> IO ()
printNearestPubs (pub, nearbyPubs) = do
  putStrLn $ printf "%s has %d nearby pubs:" (show pub) (length nearbyPubs)
  mapM_ (\p -> putStrLn $ "  " ++ show p) nearbyPubs


main :: IO ()
main = do
  pubs <- readData
  
  let nearestPubsResult = nearest pubs

  let first10 = take 10 nearestPubsResult

  putStrLn "\nNearest pubs for first 10: "
  mapM_ printNearestPubs first10
