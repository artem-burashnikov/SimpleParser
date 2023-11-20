module Parsers.AST

open Microsoft.FSharp.Reflection

type RelationalOperator =
    | LessThanOrEqual
    | NotEqual
    | GreaterThanOrEqual
    | Equal
    | GreaterThan
    | LessThan

    override this.ToString() =
        match this with
        | LessThanOrEqual -> "<="
        | NotEqual -> "<>"
        | GreaterThanOrEqual -> ">="
        | Equal -> "="
        | GreaterThan -> ">"
        | LessThan -> "<"

    /// <summary>
    /// Gets an array of all available relational operator's keywords.
    /// </summary>
    static member All() =
        let cases = FSharpType.GetUnionCases(typeof<RelationalOperator>)

        [| for case in cases do
               let operator = FSharpValue.MakeUnion(case, [||]) :?> RelationalOperator
               yield operator.ToString() |]

    static member FromString str =
        match str with
        | "<=" -> LessThanOrEqual
        | "<" -> LessThan
        | "<>" -> NotEqual
        | "=" -> Equal
        | ">" -> GreaterThan
        | ">=" -> GreaterThanOrEqual
        | _ -> failwith "Not a boolean operator"

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
