module Runner where

import qualified Compiler
import qualified JS
import qualified Parser
import Text.Megaparsec (errorBundlePretty)

compile :: String -> Either String String
compile input = case JS.moduleToString . Compiler.moduleToJS <$> Parser.parseModule "stdin" input of
  Left e -> Left $ errorBundlePretty e
  Right s -> Right s
