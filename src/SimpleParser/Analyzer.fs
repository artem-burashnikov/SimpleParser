module SimpleParser.Analyzer

open AST

let optimize context statements =

    let rec inner context expression =
        match expression with
        | Number n -> Number n
        | Multiply expressionList -> Multiply(List.map (inner context) expressionList)
        | Add expressionList -> Add(List.map (inner context) expressionList)
        | IfThenElse(conditional, trueBranch, elseBranch) ->
            match conditional with
            | True -> inner context trueBranch
            | False -> inner context elseBranch
            | BooleanExpression(operator, lhs, rhs) -> IfThenElse(BooleanExpression(operator, inner context lhs, inner context rhs), inner context trueBranch, inner context elseBranch)
        | Var varName -> Var varName

    let eliminateRedundantIfThenElse context statement =
        match statement with
        | VarAssignment(varName, value) -> VarAssignment(varName, inner context value)
        | Print expression -> Print(inner context expression)

    List.map (eliminateRedundantIfThenElse context) statements
