module Main where

import System.Environment
import Control.Monad (when)
import Text.Printf (printf)

import MyLib

main :: IO ()
main = do
  args <- getArgs
  let (config, dirs) = parseArgs args
  (_, logOutput, finalState) <- runDuM (mapM_ du dirs) config (AppState 0)
  putStrLn logOutput
  print finalState
  when (humanReadable config) $
    putStrLn $ "Total space: " ++ formatHumanReadable (totalSpace finalState)

parseArgs :: [String] -> (AppConfig, [FilePath])
parseArgs args = loop (AppConfig 0 False False, []) args
  where
    loop (config, dirs) [] = (config, reverse dirs)
    loop (config, dirs) (arg:rest) =
      case arg of
        "-d" -> loop (config {depth = read (head rest)}, dirs) (tail rest)
        "-s" -> loop (config {depth = 0}, dirs) rest
        "-h" -> loop (config {humanReadable = True}, dirs) rest
        "-v" -> loop (config {verbose = True}, dirs) rest
        dir -> loop (config, dir : dirs) rest

formatHumanReadable :: Integer -> String
formatHumanReadable bytes
  | bytes < 1024      = show bytes ++ " B"
  | bytes < 1024^2    = printf "%.2f KB" (fromIntegral bytes / 1024 :: Double)
  | bytes < 1024^3    = printf "%.2f MB" (fromIntegral bytes / 1024^2 :: Double)
  | bytes < 1024^4    = printf "%.2f GB" (fromIntegral bytes / 1024^3 :: Double)
  | otherwise         = printf "%.2f TB" (fromIntegral bytes / 1024^4 :: Double)
