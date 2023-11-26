module SimpleParser.Analyzer

open SimpleParser.Definitions

let removeUnusedVars (context: Context) statements =
    let rec _go (context: Context) expr =
        match expr with
        | Var(varName, varType) ->
            if not (context.VariablesCtx.ContainsKey varName) then
                context.VariablesCtx.Add (varName, (varType, None))
        | BooleanExpr (Expression(_,  lhs, rhs)) ->
            _go context lhs
            _go context rhs
        | Multiply exprList
        | Add exprList -> List.iter (_go context) exprList
        | IfThenElse (True, trueBranch, _) -> _go context trueBranch
        | IfThenElse (False, _, elseBranch) -> _go context elseBranch
        | IfThenElse (Expression(_, lhs, rhs), trueBranch, elseBranch) ->
            _go context lhs
            _go context rhs
            _go context trueBranch
            _go context elseBranch
        | _ -> ()

    let folder stmt clearedStmtsList =
        match stmt with
        | VarAssignment(varName, expr) ->
            if context.VariablesCtx.ContainsKey varName then
                _go context expr
                VarAssignment(varName, expr) :: clearedStmtsList
            else
                clearedStmtsList
        | Print expr ->
            _go context expr
            (Print expr) :: clearedStmtsList

    List.foldBack folder statements []

let optimize (context: Context) statements =
    let rec inner (context: Context) expression =
        match expression with
        | Number n -> Number n
        | BooleanExpr boolVal ->
            match boolVal with
            | True
            | False ->
                context.ExprType <- Boolean
                BooleanExpr boolVal
            | Expression(op, lhs, rhs) ->
                context.ExprType <- Boolean
                let analyzedLhs = inner context lhs
                let analyzedRhs = inner context rhs
                BooleanExpr(Expression(op, analyzedLhs, analyzedRhs))
        | Multiply [ x ] -> inner context x
        | Multiply (hd :: tl) ->
            context.ExprType <- Integer
            let analyzedHead = inner context hd
            let analyzedTail = List.map (inner context) tl
            Multiply (analyzedHead :: analyzedTail)
        | Add [ x ] -> inner context x
        | Add (hd :: tl) ->
            context.ExprType <- Integer
            let analyzedHead = inner context hd
            let analyzedTail = List.map (inner context) tl
            Add (analyzedHead :: analyzedTail)
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
                | Integer, _
                | Undefined, Integer
                | Undefined, Boolean -> Var(varName, Integer)
                | Boolean, Undefined -> Var(varName, Boolean)
                | Undefined, Undefined -> Var(varName, Undefined)
                | Boolean, Integer -> failwith "Boolean var and Integer expr are not supported."
                | Boolean, Boolean -> failwith "Boolean var and Boolean expr are not supported."

        | e -> failwith $"Unhandled exception in optimize. Expression type: %A{context.ExprType} Expression that caused en error:\n{e}"

    let optimizeStatements (context: Context) statement =
        match statement with
        | VarAssignment(varName, value) ->
            let evaluatedValue = inner context value
            VarAssignment(varName, evaluatedValue)
        | Print expression ->
            let evaluatedValue = inner context expression
            Print(evaluatedValue)

    List.map (optimizeStatements context) (removeUnusedVars context statements)
