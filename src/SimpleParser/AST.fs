module SimpleParser.AST

open System.Collections.Generic
open Microsoft.FSharp.Reflection

type VarType =
    | Integer
    | Boolean

    override this.ToString() =
        match this with
        | Integer -> "integer"
        | Boolean -> "boolean"

type Context<'a> = VarType option * Dictionary<string, 'a>

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

        [|
            for case in cases do
                let operator = FSharpValue.MakeUnion(case, [||]) :?> RelationalOperator
                yield operator.ToString()
        |]

    static member FromString str =
        match str with
        | "<=" -> LessThanOrEqual
        | "<" -> LessThan
        | "<>" -> NotEqual
        | "=" -> Equal
        | ">" -> GreaterThan
        | ">=" -> GreaterThanOrEqual
        | _ -> failwith "Not a boolean operator"

and BooleanValue =
    | True
    | False
    | BooleanExpression of RelationalOperator * SourceExpr * SourceExpr

and SourceExpr =
    | Number of int
    | BooleanExpr of BooleanValue
    | Multiply of list<SourceExpr>
    | Add of list<SourceExpr>
    | IfThenElse of condition: BooleanValue * trueBranch: SourceExpr * elseBranch: SourceExpr
    | Var of string * VarType

type SourceAst =
    | VarAssignment of string * SourceExpr
    | Print of SourceExpr
