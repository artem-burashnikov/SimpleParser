module Parsers.AST

type RelationalOperator =
    | LessThanOrEqual
    | LessThan
    | NotEqual
    | Equal
    | GreaterThan
    | GreaterThanOrEqual

    override this.ToString() =
        match this with
        | LessThanOrEqual -> "<="
        | LessThan -> "<"
        | NotEqual -> "<>"
        | Equal -> "="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="

and Conditional =
    | True
    | False
    | BooleanExpression of RelationalOperator * SourceExpr * SourceExpr

and SourceExpr =
    | Number of int
    | Multiply of list<SourceExpr>
    | Add of list<SourceExpr>
    | IfThenElse of condition: Conditional * trueBranch: SourceExpr * elseBranch: SourceExpr
    | Var of string

type SourceAst =
    | VarAssignment of string * SourceExpr
    | Print of SourceExpr
