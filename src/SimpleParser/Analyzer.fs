module SimpleParser.Analyzer

open SimpleParser.Definitions

let optimize (context: Context) statements =
    let rec inner (context: Context) expression =
        match expression with
        | Number n -> Number n
        | BooleanExpr boolVal ->
            match boolVal with
            | True
            | False when context.ExprType <> Integer ->
                context.ExprType <- Boolean
                BooleanExpr boolVal
            | Expression(op, lhs, rhs) ->
                let analyzedLhs = inner context lhs
                let analyzedRhs = inner context rhs
                BooleanExpr(Expression(op, analyzedLhs, analyzedRhs))
            | _ -> failwith "Type integer and boolean value in boolean expression don't match."
        | Multiply [ x ] -> inner context x
        | Multiply [ lhs; rhs ] ->
            let analyzedLhs = inner context lhs
            let analyzedRhs = inner context rhs
            context.ExprType <- Integer
            Multiply [analyzedLhs; analyzedRhs]
        | Add [ x ] -> inner context x
        | Add [ lhs; rhs ] ->
            let analyzedLhs = inner context lhs
            let analyzedRhs = inner context rhs
            context.ExprType <- Integer
            Add [analyzedLhs; analyzedRhs]
        | IfThenElse(conditional, trueBranch, elseBranch) ->
            match conditional with
            | True -> inner context trueBranch
            | False -> inner context elseBranch
            | Expression(operator, lhs, rhs) ->
                let analyzedLhs = inner context lhs
                let analyzedRhs = inner context rhs
                let optimizedTrueBranch = inner context trueBranch
                let optimizedElseBranch = inner context elseBranch
                IfThenElse(Expression(operator, analyzedLhs, analyzedRhs), optimizedTrueBranch, optimizedElseBranch)
        | Var(varName, ownVarType) ->
            if context.VariablesCtx.ContainsKey varName then
                Var(varName, fst context.VariablesCtx[varName])
            else
                match ownVarType, context.ExprType with
                | _, Undefined -> Var(varName, ownVarType)
                | Undefined, exprType -> Var(varName, exprType)
                | Integer, Integer -> Var(varName, Integer)
                | Boolean, Boolean -> Var(varName, Boolean)
                | _ -> failwith $"The type {ownVarType.ToString()} of variable {varName} doesn't match the inferred type of expression {context.ExprType}."
        | e -> failwith $"Unhandled exception. Expression that caused en error:\n{e}"

    let optimizeStatements (context: Context) statement =
        match statement with
        | VarAssignment(varName, value) ->
            let evaluatedValue = inner context value
            VarAssignment(varName, evaluatedValue)
        | Print expression ->
            let evaluatedValue = inner context expression
            Print(evaluatedValue)

    List.map (optimizeStatements context) statements
