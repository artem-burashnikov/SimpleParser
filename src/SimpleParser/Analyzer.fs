module SimpleParser.Analyzer

open SimpleParser.Definitions

let optimize (context: Context<_>) statements =

    let isIntOrNone =
        fun varType ->
            varType = Some Integer
            || varType = None

    let isBoolOrNone =
        fun varType ->
            varType = Some Boolean
            || varType = None

    let rec inner (varType: VarType option) expression =
        match expression with
        | Number n when isIntOrNone varType -> Number n, Some Integer
        | BooleanExpr boolVal when isBoolOrNone varType -> BooleanExpr boolVal, Some Boolean
        | Multiply exprList when isIntOrNone varType ->
            let optimizedExprList, inferredType = List.mapFold inner varType exprList
            Multiply optimizedExprList, inferredType
        | Add exprList when isIntOrNone varType ->
            let optimizedExprList, inferredType = List.mapFold inner varType exprList
            Add optimizedExprList, inferredType
        | IfThenElse(conditional, trueBranch, elseBranch) ->
            match conditional with
            | True -> inner varType trueBranch
            | False -> inner varType elseBranch
            | Expression(operator, lhs, rhs) ->
                let optimizedLhs, _ = inner varType lhs
                let optimizedRhs, _ = inner varType rhs
                let optimizedTrueBranch, inferredTypeFromThen = inner varType trueBranch
                let optimizedElseBranch, _ = inner inferredTypeFromThen elseBranch
                IfThenElse(Expression(operator, optimizedLhs, optimizedRhs), optimizedTrueBranch, optimizedElseBranch), inferredTypeFromThen
        | Var(varName, ownVarType) ->
            match ownVarType, varType with
            | ownVarType, None -> Var(varName, ownVarType), Some ownVarType
            | Integer, Some Integer
            | Boolean, Some Boolean -> Var(varName, ownVarType), varType
            | _ -> failwith $"The type {ownVarType.ToString()} of variable {varName} doesn't match the inferred type of expression."

        | _ when varType.IsSome -> failwith $"Expression types don't match. Expected {varType.Value.ToString()}"
        | e -> failwith $"Unhandled exception. Expression that caused en error:\n{e}"

    let optimizeStatements (context: Context<_> as (varType, _)) statement =

        match statement with
        | VarAssignment(varName, value) ->
            VarAssignment(
                varName,
                inner varType value
                |> fst
            )
        | Print expression ->
            Print(
                inner varType expression
                |> fst
            )

    List.map (optimizeStatements context) statements
