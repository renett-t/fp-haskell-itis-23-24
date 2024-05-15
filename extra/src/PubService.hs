module PubService (readData, nearest) where

import PubDomain
import CSVParsing
import RowsConversions
import EuclideanDistance

import Data.List (sortOn, maximumBy, minimumBy)
import Data.Ord (comparing)
import Control.Monad (forM_)
import Debug.Trace (trace)


-- Reading Pubs from CSV file. Parameters `filePath`, `delimiter` hardcoded
-- sorry for too much trace statements ;D
readData :: IO [Pub]
readData = do
  let filePath = "open_pubs.csv"
      delimiter = ","
      settings = CSVParsingSettings delimiter

  csvFileContent <- readFile filePath

  case parseCSV settings csvFileContent of
    Left err 
      -> do
        trace ("\nCannot parse CSV. Error: " ++ show err) $ return []
    Right csv 
      -> do
        trace "\nCSV parsed successfully." $ return ()
        trace ("Total number of CSV rows: " ++ show (length (rows csv))) $ return ()

        -- putStrLn "CSV rows: "
        -- forM_ (rows csv) print

        let rowsWithoutHeader = tail (rows csv)

        let allRows = filterValidDataRows (csvRowsToDataRows rowsWithoutHeader)
        -- putStrLn "All rows: "
        -- forM_ allRows print
        --
        -- let first10Rows = take 10 allRows
        -- putStrLn "First 10 Rows: "
        -- forM_ first10Rows print
        --
        trace ("Total number of Row instances: " ++ show (length allRows)) $ return ()

        let pubs = map convertRowToPub allRows

        trace ("Total number of Pub instances: " ++ show (length pubs)) $ return ()

        return pubs


-- Finding nearest pubs for each pub
{- 
  2) для каждого заведения вернёт список тех, которые на расстоянии меньшем 0.01° (считаем обычное евклидово расстояние, координаты - lon и lat)
  3) или, если для заведения на этом расстоянии нет других, выдаст три ближайших
 -}
nearest :: [Pub] -> [(Pub, [Pub])]
nearest pubs = map (\pub -> (pub, findNearestPubs pub pubs)) pubs

findNearestPubs :: Pub -> [Pub] -> [Pub]
findNearestPubs pub allPubs =
  let threshold = 0.01
      nearbyPubs = filter (\p -> p /= pub && euclideanDistance (geoLoc pub) (geoLoc p) < threshold) allPubs
  in if null nearbyPubs
     then take 3 $ sortPubsByDistance pub (filter (/= pub) allPubs)
     else nearbyPubs

sortPubsByDistance :: Pub -> [Pub] -> [Pub]
sortPubsByDistance pub = sortOn (euclideanDistance (geoLoc pub) . geoLoc)
