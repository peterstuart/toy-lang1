module Parser where

import AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

sc :: Parser ()
sc = Lexer.space (void $ some (char ' ')) empty empty

scn :: Parser ()
scn = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser String
semicolon = symbol ";"

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

dot :: Parser String
dot = symbol "."

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$> (char '"' *> manyTill Lexer.charLiteral (char '"'))

float :: Parser Double
float = lexeme Lexer.float

integer :: Parser Int
integer = lexeme Lexer.decimal

numberLiteral :: Parser Expression
numberLiteral = NumberLiteral <$> (try float <|> (fromIntegral <$> integer))

reserved :: [String]
reserved = ["let", "in", "if", "then", "else", "true", "false"]

identifier :: Parser String
identifier = do
  name <- lexeme ((:) <$> letterChar <*> many alphaNumChar)
  if name `elem` reserved
    then fail "fail!"
    else return name

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` comma

semiSep1 :: Parser a -> Parser [a]
semiSep1 p = p `sepBy1` semicolon

boolLiteral :: Parser Expression
boolLiteral = true <|> false
  where
    true = symbol "true" $> BoolLiteral True
    false = symbol "false" $> BoolLiteral False

variable :: Parser Expression
variable = Variable <$> identifier

binding :: Parser Binding
binding = Lexer.lineFold scn $ \sc' -> do
  v <- identifier
  _ <- Lexer.symbol sc' "="
  e <- expression
  return $ Binding v e

letExpression :: Parser Expression
letExpression = do
  b <- indentedBindings
  e <- inExpr
  return $ Let b e
  where
    indentedBindings =
      Lexer.indentBlock
        scn
        (symbol "let" $> Lexer.IndentSome Nothing pure binding)
    inExpr = Lexer.lineFold scn $ \sc' ->
      Lexer.symbol sc' "in" *> expression

function :: Parser Expression
function = Lexer.lineFold scn $ \sc' -> do
  _ <- symbol "\\"
  paramNames <- many identifier
  _ <- Lexer.symbol sc' "->"
  e <- expression
  return $ Function paramNames e

ifThenElse :: Parser Expression
ifThenElse = do
  _ <- symbol "if"
  predicate <- expression
  _ <- symbol "then"
  consequent <- expression
  _ <- symbol "else"
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

expression :: Parser Expression
expression = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable = [[binary "" FunctionApplication]]
  where
    binary name f = InfixL (f <$ symbol name)

moduleParser :: Parser Module
moduleParser = Module <$> (many (Lexer.nonIndented scn binding))

parseModule :: String -> String -> Either (ParseErrorBundle String Void) Module
parseModule = runParser moduleParser
