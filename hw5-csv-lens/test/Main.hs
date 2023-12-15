import CSVParser
import Songs
import ParsingDataRows

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

testsPlainCSVParser :: TestTree
testsPlainCSVParser = testGroup "CSV Parser Tests"
  [ testGroup "HUnit Tests"
    [ testCase "Can parse using different delimiters and produce the same result" $
        let exampleCSV1 = "Name,Age,Hobby\nRegina,21,Guitar\nNastya,22,Dance\nAdil,22,Guitar\n"
            exampleCSV2 = "Name;Age;Hobby\nRegina;21;Guitar\nNastya;22;Dance\nAdil;22;Guitar\n"
            delimiter1 = ','
            delimiter2 = ';'
            settings1 = CSVParserSettings delimiter1 True
            settings2 = CSVParserSettings delimiter2 True
        in assertEqual "Parsing with different delimiters produces the same result"
                       (parseCSV delimiter1 settings1 exampleCSV1)
                       (parseCSV delimiter2 settings2 exampleCSV2)

    , testCase "Can parse CSV with header" $
        let exampleCSV = "Name,Age,Hobby\nRegina,21,Guitar\nNastya,22,Dance\nAdil,22,Guitar\n"
            delimiter = ','
            settings = CSVParserSettings delimiter True
            expectedCSV = CSV ["Name", "Age", "Hobby"]
                              [["Regina", "21", "Guitar"], ["Nastya", "22", "Dance"], ["Adil", "22", "Guitar"]]
        in assertEqual "Parsing CSV with header ok"
                       (parseCSV delimiter settings exampleCSV)
                       (Right expectedCSV)

    , testCase "Can parse CSV without header" $
        let exampleCSV = "Regina,21,Female\nAdil,22,Male\n"
            delimiter = ','
            settings = CSVParserSettings delimiter False
            expectedCSV = CSV [] [["Regina", "21", "Female"], ["Adil", "22", "Male"]]
        in assertEqual "Parsing CSV without header ok"
                       (parseCSV delimiter settings exampleCSV)
                       (Right expectedCSV)
    ]

  ]

testsForDataRowsAndAlbums :: TestTree
testsForDataRowsAndAlbums = testGroup "DataRows to Albums Tests"
  [ testCase "Convert [DataRow] to [Album] with correct [Track]" $
      let dataRows = [DataRow "1" "Track 1" "11" "Album 1" "2023-09-11" 300000
                     , DataRow "2" "Track 2" "11" "Album 1" "2023-09-11" 240000
                     , DataRow "3" "Track 3" "12" "Album 2" "2023-12-01" 180000]

          expectedAlbums = [Album "11" "Album 1" "2023-09-11" [Track "1" "Track 1" 300000, Track "2" "Track 2" 240000]
                           , Album "12" "Album 2" "2023-12-01" [Track "3" "Track 3" 180000]]

          actualAlbums = dataRowsToAlbums dataRows
      in assertEqual "Conversion from DataRow to Album" expectedAlbums actualAlbums
  ]

tests :: TestTree
tests = testGroup "All Tests"
  [ testsPlainCSVParser
  , testsForDataRowsAndAlbums
  ]

main :: IO ()
main = defaultMain tests