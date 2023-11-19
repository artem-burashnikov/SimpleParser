module Parsers.AST

type BooleanOperator =
    | LessThanOrEqual
    | LessThan
    | NotEqual
    | Equal
    | GreaterThan
    | GreaterThanOrEqual

and BooleanExpression =
    | True
    | False
    | Expression of BooleanOperator * SourceExpr * SourceExpr

and SourceExpr =
    | Number of int
    | Multiply of list<SourceExpr>
    | Add of list<SourceExpr>
    | IfThenElse of condition: BooleanExpression * trueBranch: SourceExpr * elseBranch: SourceExpr
    | Var of string

type SourceAst =
    | VarAssignment of string * SourceExpr
    | Print of SourceExpr
