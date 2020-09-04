import Data.ByteString.Lazy.Char8 (pack)
import Runner (compile)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  tlFiles <- findByExtension [".tl"] "./test/golden"
  return $
    testGroup
      "golden tests"
      [ goldenVsString
          (takeBaseName tlFile)
          jsFile
          (pack . resultToString . compile <$> readFile tlFile)
        | tlFile <- tlFiles,
          let jsFile = replaceExtension tlFile ".js"
      ]

resultToString :: Either String String -> String
resultToString e = case e of
  Left s -> s
  Right s -> s
