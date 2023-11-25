module SimpleParser.Interpreter

open System.Collections.Generic
open SimpleParser.Definitions
open SimpleParser.Analyzer

let rec eval (context: Context<_> as (_, ctx)) expr =
    match expr with
    | Number num -> num
    | Multiply lst ->
        lst
        |> List.map (eval context)
        |> List.reduce (*)
    | Add lst ->
        lst
        |> List.map (eval context)
        |> List.reduce (+)
    | Var(varName, _) ->
        if ctx.ContainsKey varName then
            ctx[varName]
        else
            failwithf $"Var with name {varName} is not declared."
    | IfThenElse(condition: BooleanValue, trueBranch, elseBranch) ->
        let evaluatedCondition =
            match condition with
            | True -> true
            | False -> false
            | BooleanExpression(operator, lhs, rhs) ->
                let computations = [|
                    async { return eval context lhs }
                    async { return eval context rhs }
                |]

                let result =
                    computations
                    |> Async.Parallel
                    |> Async.RunSynchronously

                let evaluatedLhs, evaluatedRhs = result[0], result[1]

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

        if evaluatedCondition then
            (eval context trueBranch)
        else
            (eval context elseBranch)
    | BooleanExpr _ -> failwith "Boolean and Int don't match"

let rec evalStmt (context: Context<_> as (varType, _)) stmt =

    match stmt with
    | VarAssignment(varName, expr) when varType = Some Integer -> Some(varName, eval context expr)
    | Print expr when varType = Some Integer ->
        printfn $"{eval context expr}"
        None
    | _ -> failwith "Can't evaluate statement. Missing type information."


let evalProgram (statements: list<SourceAst>) =

    let ctx = Context(None, Dictionary<string, int>())

    List.fold
        (fun (context: Context<_> as (_, ctx)) stmt ->

            let res = evalStmt context stmt

            match res with
            | Some(varName, res) ->
                if ctx.ContainsKey varName then
                    ctx[varName] <- res
                else
                    ctx.Add(varName, res)
            | None -> ()

            context
        )

        ctx

        (optimize ctx statements)

    |> ignore
