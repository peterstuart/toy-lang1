module Parser where

import AST
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

languageDef :: Token.LanguageDef ()
languageDef =
  Token.LanguageDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.nestedComments = True,
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.reservedNames = ["let", "in", "if", "then", "else", "true", "false"],
      Token.reservedOpNames = ["", "=", "\\", "->"],
      Token.caseSensitive = True
    }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Token.semiSep1 lexer

numberLiteral :: Parser Expression
numberLiteral =
  NumberLiteral . either fromInteger id
    <$> Token.naturalOrFloat lexer

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$> Token.stringLiteral lexer

boolLiteral :: Parser Expression
boolLiteral = true <|> false
  where
    true = reserved "true" $> BoolLiteral True
    false = reserved "false" $> BoolLiteral False

variable :: Parser Expression
variable = Variable <$> identifier

letExpression :: Parser Expression
letExpression = do
  _ <- reserved "let"
  b <- commaSep1 binding
  _ <- reserved "in"
  e <- expression
  return $ Let b e
  where
    binding = do
      v <- identifier
      _ <- reservedOp "="
      e <- expression
      return $ Binding v e

function :: Parser Expression
function = do
  _ <- reservedOp "\\"
  paramNames <- many1 identifier
  _ <- reservedOp "->"
  e <- expression
  return $ Function paramNames e

ifThenElse :: Parser Expression
ifThenElse = do
  _ <- reserved "if"
  predicate <- expression
  _ <- reserved "then"
  consequent <- expression
  _ <- reserved "else"
  alternate <- expression
  return $ IfThenElse predicate consequent alternate

term :: Parser Expression
term =
  choice $
    try
      <$> [ numberLiteral,
            boolLiteral,
            stringLiteral,
            variable,
            letExpression,
            function,
            ifThenElse,
            parens expression
          ]

table :: OperatorTable String () Identity Expression
table = [[binary "" FunctionApplication AssocLeft]]
  where
    binary name fun = Infix (do reservedOp name; return fun)

expression :: Parser Expression
expression = buildExpressionParser table term

moduleParser :: Parser Module
moduleParser = Module <$> semiSep1 binding
  where
    binding = do
      name <- identifier
      _ <- reservedOp "="
      e <- expression
      return (name, e)

parseModule :: String -> Either ParseError Module
parseModule = runParser moduleParser () "stdin"
