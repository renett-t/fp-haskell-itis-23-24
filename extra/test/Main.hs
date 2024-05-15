module Main (main) where

import PubDomain
import CSVParsing
import RowsConversions

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ 
  	testsForCSVParsing
  , testsForRowsConversion
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

