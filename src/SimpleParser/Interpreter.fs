module SimpleParser.Interpreter

open System.Collections.Generic
open SimpleParser.AST

let rec eval (context: Dictionary<_, _>) expr =
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
    | Var varName ->
        if context.ContainsKey varName then
            context[varName]
        else
            failwithf $"Var with name {varName} is not declared."
    | IfThenElse(condition: Conditional, trueBranch, elseBranch) ->
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

let rec evalStmt context stmt =
    match stmt with
    | VarAssignment(varName, expr) -> Some(varName, eval context expr)
    | Print expr ->
        printfn $"{eval context expr}"
        None

let evalProgram (statements: list<SourceAst>) =
    let context = Dictionary<string, int>()

    List.fold
        (fun (context: Dictionary<_, _>) stmt ->
            let res = evalStmt context stmt

            match res with
            | Some(varName, res) ->
                if context.ContainsKey varName then
                    context[varName] <- res
                else
                    context.Add(varName, res)
            | None -> ()

            context
        )
        context
        statements
    |> ignore
