module Main (main) where

import PubDomain
import PubService
import CSVParsing
import RowsConversions

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testsForCSVParsing
  , testsForRowsConversion
  , testsForNearestPubs
  ]

testsForCSVParsing :: TestTree
testsForCSVParsing = testGroup "CSV Parsing Tests"
    [ testGroup "Single Cell"
        [ testCase "Parses a single cell (quoted)" $ do
            let settings = CSVParsingSettings ","
            let csvContent = "\"id\"\n"
            let result = parseCSV settings csvContent
            result @?= Right (CSV [["id"]])

        , testCase "Parses a single cell (unquoted)" $ do
            let settings = CSVParsingSettings ","
            let csvContent = "id\n"
            let result = parseCSV settings csvContent
            result @?= Right (CSV [["id"]])
        ]

    , testGroup "Single Row"
        [ testCase "Parses a single row (mixed quoted and unquoted cells)" $ do
            let settings = CSVParsingSettings ","
            let csvContent = "\"id\",name,\"city\"\n"
            let result = parseCSV settings csvContent
            result @?= Right (CSV [["id", "name", "city"]])
        ]

    , testGroup "Multiple Rows"
        [ testCase "Parses multiple rows (mixed quoted and unquoted cells)" $ do
            let settings = CSVParsingSettings ","
            let csvContent = "id,name,city\n1,\"Regina\",Kazan\n"
            let result = parseCSV settings csvContent
            result @?= Right (CSV [["id", "name", "city"], ["1", "Regina", "Kazan"]])
        ]
    ]


testsForRowsConversion :: TestTree
testsForRowsConversion = testGroup "Tests for Rows Conversion"
    [ testCase "Convert CSV rows to Data Rows" $
        let csvRows = [["1", "ABC", "Pub1", "Address1", "12345", "1.23", "4.56", "7.89", "10.11", "Town1"]
                      ,["2", "DEF", "Pub2", "Address2", "23456", "2.34", "5.67", "8.91", "12.13", "Town2"]]
            dataRows = [Right Row {_uid = 1, fsa = "ABC", _name = "Pub1", address = "Address1", postcode = "12345", easting = 1.23, northing = 4.56, _lat = 7.89, _lon = 10.11, town = "Town1"}
                       ,Right Row {_uid = 2, fsa = "DEF", _name = "Pub2", address = "Address2", postcode = "23456", easting = 2.34, northing = 5.67, _lat = 8.91, _lon = 12.13, town = "Town2"}]
        in csvRowsToDataRows csvRows @?= dataRows

    , testCase "Filter valid Rows" $
        let rows = [Right Row {_uid = 1, fsa = "ABC", _name = "Pub1", address = "Address1", postcode = "12345", easting = 1.23, northing = 4.56, _lat = 7.89, _lon = 10.11, town = "Town1"}
                   ,Right Row {_uid = 2, fsa = "DEF", _name = "Pub2", address = "Address2", postcode = "23456", easting = 2.34, northing = 5.67, _lat = 8.91, _lon = 12.13, town = "Town2"}
                   ,Left "Invalid input: Row does not contain all required fields"
                   ,Left "Invalid input: Row does not contain all required fields"]
            validRows = [Row {_uid = 1, fsa = "ABC", _name = "Pub1", address = "Address1", postcode = "12345", easting = 1.23, northing = 4.56, _lat = 7.89, _lon = 10.11, town = "Town1"}
                        ,Row {_uid = 2, fsa = "DEF", _name = "Pub2", address = "Address2", postcode = "23456", easting = 2.34, northing = 5.67, _lat = 8.91, _lon = 12.13, town = "Town2"}]
        in filterValidDataRows rows @?= validRows
    ]


testsForNearestPubs :: TestTree
testsForNearestPubs = testGroup "Tests for Nearest Pubs"
    [ testGroup "Corner Cases"
        [ testCase "Empty [] for empty [Pubs]" $ do
            let pubs = []
            let result = nearest pubs
            let expected = []
            result @?= expected

        , testCase "Single Pub" $ do
            let pub = Pub 1 "Pub 1" (GeoLocation 0 0)
            let pubs = [pub]
            let result = nearest pubs
            let expected = [(pub, [])]
            result @?= expected
        ]

    , testGroup "Multiple Pubs"
        [ testCase "Two close Pubs" $ do
            let pub1 = Pub 1 "Pub 1" (GeoLocation 0 0)
            let pub2 = Pub 2 "Pub 2" (GeoLocation 1 1)
            let pubs = [pub1, pub2]
            let result = nearest pubs
            let expected = [(pub1, [pub2]), (pub2, [pub1])]
            result @?= expected

         , testCase "Pubs at the same location" $ do
            let pub1 = Pub 1 "Pub 1" (GeoLocation 52.3 1.0)
            let pub2 = Pub 2 "Pub 2" (GeoLocation 52.3 1.0)
            let pubs = [pub1, pub2]
            let result = nearest pubs
            let expected = [(pub1, [pub2]), (pub2, [pub1])]
            result @?= expected

        , testCase "Pubs far away from each other" $ do
            let pub1 = Pub 1 "Pub 1" (GeoLocation 0 0)
            let pub2 = Pub 2 "Pub 2" (GeoLocation 10 10)
            let pub3 = Pub 3 "Pub 3" (GeoLocation 20 20)
            let pub4 = Pub 4 "Pub 4" (GeoLocation 30 30)
            let pub5 = Pub 5 "Pub 5" (GeoLocation 40 40)
            let pubs = [pub1, pub2, pub3, pub4, pub5]
            let result = nearest pubs
            let expected = [ (pub1, [pub2, pub3, pub4])
                           , (pub2, [pub1, pub3, pub4])
                           , (pub3, [pub2, pub4, pub1])
                           , (pub4, [pub3, pub5, pub2])
                           , (pub5, [pub4, pub3, pub2])
                           ]
            result @?= expected
        ]
    ]
