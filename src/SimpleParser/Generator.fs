module SimpleParser.Generator

open SimpleParser.Definitions

let generateCode ast =
    let rec generateFromExpr expr =
        let folder separator =
            fun state next ->
                String.concat separator [
                    state
                    generateFromExpr next
                ]

        match expr with
        | Number n -> sprintf $"%d{n}"
        | Add [ x ]
        | Multiply [ x ] -> sprintf $"%s{generateFromExpr x}"
        | Var(varName, _) -> sprintf $"%s{varName}"
        | Multiply(hd :: tl) -> List.fold (folder "*") (generateFromExpr hd) tl
        | Add(hd :: tl) -> List.fold (folder "+") (generateFromExpr hd) tl
        | BooleanExpr True -> "true"
        | BooleanExpr False -> "false"
        | BooleanExpr(Expression(op, lhs, rhs)) -> sprintf $"%s{generateFromExpr lhs}%s{op.ToString()}%s{generateFromExpr rhs}"
        | IfThenElse(booleanValue, trueBranch, elseBranch) -> sprintf $"if(%s{generateFromExpr (BooleanExpr(booleanValue))})then(%s{generateFromExpr trueBranch})else(%s{generateFromExpr elseBranch})"
        | Add [] -> failwith "Addition on zero elements."
        | Multiply [] -> failwith "Multiplication on zero elements."

    let generateFromStmt stmt =
        match stmt with
        | VarAssignment(varName, expr) -> sprintf $"%s{varName}=%s{generateFromExpr expr}\n"
        | Print expr -> sprintf $"print:%s{generateFromExpr expr}\n"

    List.fold (fun state stmt -> String.concat "" [state; generateFromStmt stmt]) "" ast
