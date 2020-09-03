module Main where

import AST
import qualified Compiler
import qualified JS
import qualified Parser
import Text.Megaparsec

main :: IO ()
main = do
  input <- getContents
  putStrLn $ process input

process :: String -> String
process input =
  case JS.moduleToString
    . Compiler.moduleToJS <$> Parser.parseModule "stdin" input of
    Left e -> errorBundlePretty e
    Right s -> s
