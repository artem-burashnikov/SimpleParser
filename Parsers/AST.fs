module Parsers.AST

type BooleanOperator =
    | LessThanOrEqual
    | LessThan
    | NotEqual
    | Equal
    | GreaterThan
    | GreaterThanOrEqual

and BooleanExpression =
    | Expression of BooleanOperator * SourceExpr * SourceExpr
    | True
    | False

and SourceExpr =
    | Number of int
    | Multiply of list<SourceExpr>
    | Add of list<SourceExpr>
    | IfThenElse of condition: BooleanExpression * trueBranch: SourceExpr * elseBranch: SourceExpr
    | Var of string

type SourceAst =
    | VarAssignment of string * SourceExpr
    | Print of SourceExpr
