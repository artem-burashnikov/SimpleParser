module SimpleParser.Interpreter

open System.Collections.Generic
open Microsoft.FSharp.Core
open SimpleParser.Definitions
open SimpleParser.Analyzer

let compareTwo op lhs rhs =
    match op with
    | LessThanOrEqual ->
        lhs <= rhs
    | LessThan ->
        lhs < rhs
    | NotEqual ->
        lhs <> rhs
    | Equal ->
        lhs = rhs
    | GreaterThan ->
        lhs > rhs
    | GreaterThanOrEqual ->
        lhs >= rhs

let rec evalConditional context conditional =
    match conditional with
    | True -> true
    | False -> false
    | Expression(operator, lhs, rhs) ->
        let evaluatedLhs = evalInt context lhs
        let evaluatedRhs = evalInt context rhs

        compareTwo operator evaluatedLhs evaluatedRhs

and evalInt (context: Context) expr =
    match expr with
    | Number num -> num
    | Multiply lst when context.ExprType = Integer ->
        lst
        |> List.map (evalInt context)
        |> List.reduce (*)
    | Add lst ->
        lst
        |> List.map (evalInt context)
        |> List.reduce (+)
    | Var(varName, _) ->
        match context.VariablesCtx with
        | variablesCtx ->
            if variablesCtx.ContainsKey varName then
                unbox<int> (snd variablesCtx[varName])
            else
                failwithf $"Var with name {varName} is not declared."
    | IfThenElse(condition: BooleanValue, trueBranch, elseBranch) ->
        if evalConditional context condition then
            (evalInt context trueBranch)
        else
            (evalInt context elseBranch)
    | e -> failwith $"Boolean and Int don't match. Expression that caused a failure: {e}"

and evalBool (context: Context) expr =
    match expr with
    | Add [ x ] -> evalBool context x
    | Multiply [ x ] -> evalBool context x
    | BooleanExpr body -> evalConditional context body
    | Var(varName, _) ->
        match context.VariablesCtx, context.ExprType with
        | d, Boolean ->
            if d.ContainsKey varName then
                unbox<bool> (snd d[varName])
            else
                failwithf $"Var with name {varName} is not declared."
        | _ -> failwith $"Types don't match. {varName} is expected to be boolean."

    | IfThenElse(condition: BooleanValue, trueBranch, elseBranch) ->
        if evalConditional context condition then
            (evalBool context trueBranch)
        else
            (evalBool context elseBranch)
    | e ->
        failwith $"Boolean and Int don't match. Expression type:%A{context.ExprType}\nExpression:{e}"

let rec evalStmt (context: Context) stmt =
    match stmt with
    | VarAssignment(varName, Number n) ->
        let res = evalInt context (Number n)
        context.IntResult <- res
        IntResult(varName, res)
    | VarAssignment(varName, expr) when context.ExprType = Integer ->
        let res = evalInt context expr
        context.IntResult <- res
        IntResult(varName, res)
    | VarAssignment(varName, expr) when context.ExprType = Boolean ->
        let res = evalBool context expr
        context.BoolResult <- res
        BoolResult(varName, res)
    | Print (Number n) ->
        let res = evalInt context (Number n)
        context.IntResult <- res
        Unit
    | Print (Var _) ->
        Unit
    | Print expr when context.ExprType = Integer ->
        let res = evalInt context expr
        context.IntResult <- res
        Unit
    | Print expr when context.ExprType = Boolean ->
        let res = evalBool context expr
        context.BoolResult <- res
        Unit
    | _ -> failwith "Can't evaluate statement. Missing type information."

let evalProgram (statements: list<SourceAst>) =

    let evaluationContext = Context(Undefined, Dictionary<string, VarType * obj>())

    let optimizedStatements = optimize evaluationContext statements

    let folder (ctx: Context) (stmt: SourceAst) =
        let res = evalStmt ctx stmt

        match res with
        | IntResult(varName, evaluatedResult) ->
            if ctx.VariablesCtx.ContainsKey varName then
                ctx.VariablesCtx[varName] <- fst ctx.VariablesCtx[varName], box evaluatedResult
            else
                ctx.VariablesCtx.Add(varName, (Integer, box evaluatedResult))
        | BoolResult(varName, evaluatedResult) ->
            if ctx.VariablesCtx.ContainsKey varName then
                ctx.VariablesCtx[varName] <- fst ctx.VariablesCtx[varName], box evaluatedResult
            else
                ctx.VariablesCtx.Add(varName, (Boolean, box evaluatedResult))
        | _ -> ()

        ctx

    List.fold folder evaluationContext optimizedStatements
