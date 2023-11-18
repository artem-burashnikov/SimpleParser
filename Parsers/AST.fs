module Parsers.AST

type SourceExpr =
    | Number of int
    | Multiply of list<SourceExpr>
    | Add of list<SourceExpr>
    | Var of string

type SourceAst =
    | VarAssignment of string * SourceExpr
    | Print of SourceExpr
