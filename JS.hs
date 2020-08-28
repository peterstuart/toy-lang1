module JS where

import Data.List (intercalate)

data Expression
  = NumberLiteral Double
  | StringLiteral String
  | BoolLiteral Bool
  | Variable String
  | Function [String] [Statement] Expression
  | FunctionApplication Expression [Expression]
  | Ternary Expression Expression Expression
  deriving (Eq, Show)

data Statement
  = Let String Expression
  deriving (Eq, Show)

data Module
  = Module [Statement]
  deriving (Eq, Show)

exprToString :: Expression -> String
exprToString expr = case expr of
  NumberLiteral num ->
    show num
  StringLiteral string ->
    wrapInQuotes string
  BoolLiteral bool ->
    if bool then "true" else "false"
  Variable name ->
    name
  Function params statements expr ->
    let paramsString = intercalate ", " params
        statementsString = statements >>= statementToString
        exprString = exprToString expr
     in "((" ++ paramsString ++ ") => {" ++ statementsString ++ "return " ++ exprString ++ "})"
  FunctionApplication f params ->
    let paramsString = intercalate ", " (exprToString <$> params)
     in exprToString f ++ "(" ++ paramsString ++ ")"
  Ternary predicate consequent alternate ->
    let wrapExpr = wrapInParens . exprToString
     in wrapExpr predicate ++ " ? " ++ wrapExpr consequent ++ " : " ++ wrapExpr alternate

statementToString :: Statement -> String
statementToString statement = case statement of
  Let name value -> "const " ++ name ++ " = " ++ exprToString value ++ ";"

moduleToString :: Module -> String
moduleToString (Module bindings) = intercalate "\n\n" $ statementToString <$> bindings

wrapIn :: String -> String -> String -> String
wrapIn open close s = open ++ s ++ close

wrapInParens :: String -> String
wrapInParens = wrapIn "(" ")"

wrapInBraces :: String -> String
wrapInBraces = wrapIn "{" "}"

wrapInQuotes :: String -> String
wrapInQuotes = wrapIn "\"" "\""
