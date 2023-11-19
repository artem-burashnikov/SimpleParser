module Parsers.AST

type SourceExpr =
    | Number of int
    | Multiply of list<SourceExpr>
    | Add of list<SourceExpr>
    | BooleanExpression of Comparison
    | Var of string

and Comparison =
    | LessThanOrEqual of SourceExpr * SourceExpr
    | LessThan of SourceExpr * SourceExpr
    | NotEqual of SourceExpr * SourceExpr
    | Equal of SourceExpr * SourceExpr
    | GreaterThan of SourceExpr * SourceExpr
    | GreaterThanOrEqual of SourceExpr * SourceExpr

type SourceAst =
    | VarAssignment of string * SourceExpr
    | Print of SourceExpr