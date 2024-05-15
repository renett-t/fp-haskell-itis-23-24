module CSVParsing where

import Text.Parsec
import Text.Parsec.String (Parser)
import Debug.Trace (trace, traceShow)

data CSV = CSV { rows :: [[String]] }
  deriving (Show, Eq)

data CSVParsingSettings = CSVParsingSettings
  { delimiter :: String }

csvCell :: String -> Parser String
csvCell del = quotedCell <|> nonQuotedCell
  where
    quotedCell :: Parser String
    quotedCell = do
      char '"'
      content <- many quotedChar
      char '"'
      -- trace ("  Quoted cell content: " ++ show content) $ return content
      return content

    quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

    nonQuotedCell :: Parser String
    nonQuotedCell =  do
      content <- many (noneOf (del ++ "\n" ++ "\r" ++ "\"" ++ "\n\r" ++ "\r\n"))
      -- trace ("  Non-quoted cell content: " ++ show content) $ return content
      return content

csvRow :: CSVParsingSettings -> Parser [String]
csvRow settings = do
  cells <- csvCell (delimiter settings) `sepBy` string (delimiter settings)
  -- traceShow cells $ return cells
  return cells

csvFile :: CSVParsingSettings -> Parser CSV
csvFile settings = CSV <$> endBy (csvRow settings) eol

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- input is a String representatioin of given file
parseCSV :: CSVParsingSettings -> String -> Either ParseError CSV
parseCSV settings = parse (csvFile settings) "(unknown)"
