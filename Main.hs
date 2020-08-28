module Main where

import AST
import qualified JS
import Parser

main :: IO ()
main = putStrLn "Hello, Haskell!"

bindingToJS :: Binding -> JS.Statement
bindingToJS (Binding name expr) = JS.Let name (exprToJS expr)

exprToJS :: Expression -> JS.Expression
exprToJS expr = case expr of
  NumberLiteral num ->
    JS.NumberLiteral num
  StringLiteral string ->
    JS.StringLiteral string
  BoolLiteral bool ->
    JS.BoolLiteral bool
  Variable name ->
    JS.Variable name
  Let bindings expr ->
    JS.FunctionApplication (JS.Function [] (bindingToJS <$> bindings) (exprToJS expr)) []
  Function params expression ->
    JS.Function params [] (exprToJS expr)
  FunctionApplication f param ->
    JS.FunctionApplication (exprToJS f) [exprToJS param]
  IfThenElse predicate consequent alternate ->
    JS.Ternary (exprToJS predicate) (exprToJS consequent) (exprToJS alternate)

moduleToJS :: Module -> JS.Module
moduleToJS (Module declarations) = JS.Module $ declarationToStatement <$> declarations
  where
    declarationToStatement (name, expr) = JS.Let name (exprToJS expr)
