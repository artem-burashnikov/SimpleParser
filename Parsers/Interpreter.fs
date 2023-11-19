module Parsers.Interpreter

open System.Collections.Generic
open Parsers.AST

let rec eval (context: Dictionary<_, _>) expr =
    match expr with
    | Number num -> num
    | Multiply lst -> lst |> List.map (eval context) |> List.reduce (*)
    | Add lst -> lst |> List.map (eval context) |> List.reduce (+)
    | Var varName ->
        if context.ContainsKey varName then
            context[varName]
        else
            failwithf $"Var with name {varName} is not declared."
    | IfThenElse(condition, trueBranch, elseBranch) ->
        let evaluatedCondition =
            match condition with
            | True -> true
            | False -> false
            | Expression(operator, lhs, rhs) ->
                let evaluatedLhs = eval context lhs
                let evaluatedRhs = eval context rhs

                match operator with
                | LessThanOrEqual -> evaluatedLhs <= evaluatedRhs
                | LessThan -> evaluatedLhs < evaluatedRhs
                | NotEqual -> evaluatedLhs <> evaluatedRhs
                | Equal -> evaluatedLhs = evaluatedRhs
                | GreaterThan -> evaluatedLhs >= evaluatedRhs
                | GreaterThanOrEqual -> evaluatedLhs >= evaluatedRhs

        if evaluatedCondition then
            (eval context trueBranch)
        else
            (eval context elseBranch)

let rec evalStmt context stmt =
    match stmt with
    | VarAssignment(v_name, expr) -> Some(v_name, eval context expr)
    | Print expr ->
        printfn $"{eval context expr}"
        None

let evalProgram (statements: list<SourceAst>) =
    let context = Dictionary<string, int>()

    List.fold
        (fun (context: Dictionary<_, _>) stmt ->
            let res = evalStmt context stmt

            match res with
            | Some(v_name, res) ->
                if context.ContainsKey v_name then
                    context[v_name] <- res
                else
                    context.Add(v_name, res)
            | None -> ()

            context)
        context
        statements
    |> ignore
