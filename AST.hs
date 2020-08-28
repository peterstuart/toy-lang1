module AST where

data Binding
  = Binding String Expression
  deriving (Eq, Show)

data Expression
  = NumberLiteral Int
  | StringLiteral String
  | BoolLiteral Bool
  | Variable String
  | Let [Binding] Expression
  | Function [String] Expression
  | FunctionApplication Expression Expression
  | IfThenElse Expression Expression Expression
  deriving (Eq, Show)

data Module
  = Module [(String, Expression)]
  deriving (Eq, Show)
