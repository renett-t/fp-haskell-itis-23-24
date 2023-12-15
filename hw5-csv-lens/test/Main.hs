{-# LANGUAGE OverloadedStrings #-}

import CSVParser

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "CSV Parser Tests"
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
