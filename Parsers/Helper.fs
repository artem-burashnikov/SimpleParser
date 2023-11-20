module Parsers.Helper

open Parsers.AST

let relationalOperators = [| "<="; "<>"; ">="; "="; ">"; "<" |]

let parseKeyWrdToRelationalOp =
    fun keyword ->
        match keyword with
        | "<=" -> LessThanOrEqual
        | "<" -> LessThan
        | "<>" -> NotEqual
        | "=" -> Equal
        | ">" -> GreaterThan
        | ">=" -> GreaterThanOrEqual
        | _ -> failwith "Not a boolean operator"
