module Main where

import Runner (compile)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  input <- getContents
  case compile input of
    Left e -> hPutStrLn stderr e
    Right output -> putStrLn output
