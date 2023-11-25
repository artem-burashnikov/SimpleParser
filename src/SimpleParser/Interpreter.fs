module SimpleParser.Interpreter

open System.Collections.Generic
open Microsoft.FSharp.Core
open SimpleParser.Definitions
open SimpleParser.Analyzer

let rec evalConditional context conditional =
    match conditional with
    | True -> true
    | False -> false
    | Expression(operator, lhs, rhs) ->
        let evaluatedLhs, evaluatedRhs = evalInt context lhs, evalInt context rhs

        match operator with
        | LessThanOrEqual ->
            evaluatedLhs
            <= evaluatedRhs
        | LessThan -> evaluatedLhs < evaluatedRhs
        | NotEqual ->
            evaluatedLhs
            <> evaluatedRhs
        | Equal -> evaluatedLhs = evaluatedRhs
        | GreaterThan ->
            evaluatedLhs
            >= evaluatedRhs
        | GreaterThanOrEqual ->
            evaluatedLhs
            >= evaluatedRhs

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
        match context.VariablesCtx, context.ExprType with
        | d, Integer ->
            if d.ContainsKey varName then
                unbox<int> (snd d[varName])
            else
                failwithf $"Var with name {varName} is not declared."
        | _ -> failwith $"Types don't match. {varName} is expected to be integer."
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
    | e -> failwith $"{e}: Boolean and Int don't match"

let rec evalStmt (context: Context) stmt =
    match stmt with
    | VarAssignment(varName, expr) when context.ExprType = Integer -> IntResult(varName, evalInt context expr)
    | VarAssignment(varName, expr) when context.ExprType = Boolean -> BoolResult(varName, evalBool context expr)
    | Print expr when context.ExprType = Integer || context.ExprType = Undefined ->
        context.IntResult <- evalInt context expr
        Unit
    | Print expr when context.ExprType = Boolean ->
        context.BoolResult <- evalBool context expr
        Unit
    | _ -> failwith "Can't evaluate statement. Missing type information."

let evalProgram (statements: list<SourceAst>) =

    let globalContext = Context(Undefined, Dictionary<string, VarType * obj>())

    let optimizedStatements = optimize globalContext statements

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

    List.fold folder globalContext optimizedStatements
