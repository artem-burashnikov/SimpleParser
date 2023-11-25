module SimpleParser.Definitions

open System.Collections.Generic

type VarType =
    | Integer
    | Boolean
    | Undefined

    override this.ToString() =
        match this with
        | Integer -> "integer"
        | Boolean -> "boolean"
        | Undefined -> "undefined"

type Context =
    val mutable IntResult: int
    val mutable BoolResult: bool
    val mutable ExprType: VarType
    val VariablesCtx: Dictionary<string, VarType * obj>

    new(exprType, variablesContext) =
        {
            IntResult = 0
            BoolResult = false
            ExprType = exprType
            VariablesCtx = variablesContext
        }

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
    | Expression of RelationalOperator * SourceExpr * SourceExpr

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

type ProgramResult =
    | IntResult of string * int
    | BoolResult of string * bool
    | Unit
