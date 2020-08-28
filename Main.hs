module Main where

import AST
import qualified Compiler
import qualified JS
import qualified Parser

main :: IO ()
main = do
  input <- getContents
  putStrLn $ process input

process :: String -> String
process input =
  case JS.moduleToString
    . Compiler.moduleToJS <$> Parser.parseModule input of
    Left e -> show e
    Right s -> s
