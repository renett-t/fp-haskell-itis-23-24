module Main(main) where

import CSVParser
import Songs
import ParsingDataRows
import SongsFunctions

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testsPlainCSVParser
  , testsForDataRowsAndAlbums
  , testsForAlbumSearch
  ]


------------------------------

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



testsForAlbumSearch :: TestTree
testsForAlbumSearch = testGroup "Album Search Tests"
  [ testCase "Find longest album by total duration" $ do
      let albums = [Album "11" "Album 1" "2023-09-11" [Track "1" "Track 1" 300000, Track "2" "Track 2" 240000]
                   , Album "12" "Album 2" "2023-12-01" [Track "3" "Track 3" 180000]]
          expectedAlbum = AlbumDurationInfo (Album "11" "Album 1" "2023-09-11" [Track "1" "Track 1" 300000, Track "2" "Track 2" 240000]) 540000
          actualAlbum = findLongestAlbum albums
      assertEqual "Found longest album by total duration" (Just expectedAlbum) actualAlbum

  , testCase "Find album with longest average duration of it's tracks" $
      let track1 = Track "1_1" "Track 1" 600000
          track2 = Track "1_2" "Track 2" 500000
          track3 = Track "2_1" "Track 3" 400000
          album1 = Album "1" "Album 1" "2023-09-11" [track1, track2]
          album2 = Album "2" "Album 2" "2023-12-01" [track3]
          albums = [album1, album2]
      in findAlbumWithLongestAverageDuration albums @?= Just album1

  , testCase "Find album with shortest track" $
      let track1 = Track "1_1" "Track 1" 20000
          track2 = Track "1_2" "Track 2" 40000
          track3 = Track "2_1" "Track 3" 10000
          album1 = Album "1" "Album 1" "2023-09-11" [track1, track2]
          album2 = Album "2" "Album 2" "2023-12-01" [track3]
          albums = [album1, album2]
      in findAlbumWithShortestTrack albums @?= Just album2

  , testCase "Find albums released in a specific year" $
      let album1 = Album "1" "Album 1" "2022-01-01" []
          album2 = Album "2" "Album 2" "2023-02-15" []
          album3 = Album "3" "Album 3" "2023-01-01" []
          albums = [album1, album2, album3]
          targetYear = 2023
      in findAlbumsByYear targetYear albums @?= [album2, album3]

  , testCase "Find albums released in a specific year returns returns an empty list if no albums were released in the given year" $
      let album1 = Album "1" "Album 1" "2022-01-01" []
          album2 = Album "2" "Album 2" "2023-02-15" []
          album3 = Album "3" "Album 3" "2024-01-01" []
          albums = [album1, album2, album3]
          targetYear = 2025
      in findAlbumsByYear targetYear albums @?= []

  ]
