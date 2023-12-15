{-# LANGUAGE OverloadedStrings #-}

module CSVParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import Data.Void

data CSV = CSV { header :: [String], rows :: [[String]] }
  deriving (Show, Eq)

data CSVParserSettings = CSVParserSettings
  { delimiter :: Char
  , hasHeader :: Bool
  }

type Parser = Parsec Void String

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec.html#v:takeWhileP
cellParser :: Char -> Parser String
cellParser delimiter = takeWhileP (Just "Cell Name For Token)") (\c -> c /= delimiter && c /= '\n')

rowParser :: Char -> Parser [String]
rowParser delimiter = cellParser delimiter `sepBy` char delimiter <* eol

-- Парсер для CSV с заголовком
csvWithHeaderParser :: Char -> Parser CSV
csvWithHeaderParser delimiter = do
  headerRow <- rowParser delimiter
  rows <- Text.Megaparsec.many (rowParser delimiter)
  return $ CSV headerRow rows -- (headerRow:rows) добавит заголовок в начало списка строк, в принципе необязательно

-- Парсер для CSV без заголовка
csvWithoutHeaderParser :: Char -> Parser CSV
csvWithoutHeaderParser delimiter = do
  rows <- Text.Megaparsec.many (rowParser delimiter)
  return $ CSV [] rows

csvParser :: Char -> CSVParserSettings -> Parser CSV
csvParser delimiter settings = if hasHeader settings
  then csvWithHeaderParser delimiter
  else csvWithoutHeaderParser delimiter

parseCSV :: Char -> CSVParserSettings -> String -> Either (ParseErrorBundle String Void) CSV
parseCSV delimiter settings = parse (csvParser delimiter settings) ""
