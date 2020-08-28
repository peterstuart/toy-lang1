module Parser where

import AST
import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.String

letKeyword :: String
letKeyword = "let"

inKeyword :: String
inKeyword = "in"

ifKeyword :: String
ifKeyword = "if"

thenKeyword :: String
thenKeyword = "then"

elseKeyword :: String
elseKeyword = "else"

trueKeyword :: String
trueKeyword = "true"

falseKeyword :: String
falseKeyword = "false"

keywords :: [String]
keywords =
  [ letKeyword,
    inKeyword,
    ifKeyword,
    thenKeyword,
    elseKeyword,
    trueKeyword,
    falseKeyword
  ]

numberLiteral :: Parser Expression
numberLiteral = NumberLiteral . read <$> many1 digit

stringLiteral :: Parser Expression
stringLiteral =
  StringLiteral
    <$> between
      (char '"')
      (char '"')
      (many $ noneOf "\"")

boolLiteral :: Parser Expression
boolLiteral = true <|> false
  where
    true = keyword trueKeyword $> BoolLiteral True
    false = keyword falseKeyword $> BoolLiteral False

variableString :: Parser String
variableString = do
  firstChar <- identifierFirstChar
  rest <- many identifierChar
  let s = firstChar : rest
  if s `elem` keywords
    then fail $ "illegal variable name: " ++ s
    else return s

variable :: Parser Expression
variable = Variable <$> variableString

letExpression :: Parser Expression
letExpression = do
  _ <- keyword letKeyword
  _ <- many1 whitespace
  b <- sepBy1 binding (many1 newline)
  _ <- many1 whitespace
  _ <- keyword inKeyword
  _ <- many1 whitespace
  e <- expression
  return $ Let b e
  where
    binding = do
      v <- variableString
      _ <- many whitespace
      _ <- char '='
      _ <- many whitespace
      e <- expression
      return $ Binding v e

function :: Parser Expression
function = do
  _ <- char '\\'
  _ <- many whitespace
  paramNames <- sepEndBy1 variableString (many1 whitespace)
  _ <- string "->"
  _ <- many whitespace
  e <- expression
  return $ Function paramNames e

functionApplication :: Parser Expression
functionApplication = chainl1 expression op
  where
    op = do
      _ <- char ' '
      return FunctionApplication

ifThenElse :: Parser Expression
ifThenElse = do
  _ <- keyword ifKeyword
  _ <- many1 whitespace
  predicate <- expression
  _ <- many1 whitespace
  consequent <- expression
  _ <- many1 whitespace
  alternate <- expression
  return $ IfThenElse predicate consequent alternate

expression :: Parser Expression
expression = choice $ try <$> (functionApplication : nonFunctionApplicationExpressions)

nonFunctionApplicationExpressions :: [Parser Expression]
nonFunctionApplicationExpressions =
  [ numberLiteral,
    boolLiteral,
    variable,
    letExpression,
    function
  ]

identifierFirstChar :: Parser Char
identifierFirstChar = oneOf ['a' .. 'z']

identifierChar :: Parser Char
identifierChar = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

anyKeyword :: Parser ()
anyKeyword = choice (string <$> keywords) $> ()

keyword :: String -> Parser ()
keyword k = string k *> notFollowedBy identifierChar

whitespace :: Parser ()
whitespace = (space <|> newline) $> ()
--   f x  y
--  (f x) y
-- ((f x) y)
