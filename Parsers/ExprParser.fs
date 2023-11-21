module Parsers.ExprParser

open Parsers.Combinators
open Parsers.AST

let parseIdentifier: Parser<string> =
    parseSome (satisfy (fun x -> List.contains x [ 'a' .. 'z' ]))
    |> fMap (fun res -> res |> Array.ofList |> System.String)

let parseNumber =
    parseSeq (satisfy (fun x -> List.contains x [ '1' .. '9' ])) (fun res ->
        fMap (fun tl -> res :: tl) (parseMany (satisfy (fun x -> List.contains x [ '0' .. '9' ]))))
    |> fMap (fun res -> res |> Array.ofList |> System.String |> int |> Number)

let parseMultiply =
    parseList (parseAlt parseNumber (fMap Var parseIdentifier)) (parseIgnore (parseChar '*'))
    |> fMap Multiply

let parseAdd = parseList parseMultiply (parseIgnore (parseChar '+')) |> fMap Add

let parseKeyWordPrint = parseKeyWord "print"

let parsePrint =
    parseSeq parseKeyWordPrint (fun _ -> parseSeq (parseChar ':') (fun _ -> fMap Print parseAdd))

let parseBooleanTrue =
    parseSeq (parseIgnore (parseKeyWord "true")) (fun _ -> fMap (fun _ -> True) parseEpsilon)

let parseBooleanFalse =
    parseSeq (parseIgnore (parseKeyWord "false")) (fun _ -> fMap (fun _ -> False) parseEpsilon)

let parseBooleanValue = parseAlt parseBooleanTrue parseBooleanFalse

let parseConditionalExpression =
    let mapper relationalOperator =
        parseList parseAdd (parseIgnore (parseKeyWord relationalOperator))
        |> fMap (fun sourceExprList ->
            if List.length sourceExprList = 2 then
                let lhs :: rhs :: _ = sourceExprList
                
                BooleanExpression(RelationalOperator.FromString relationalOperator, lhs, rhs) |> Ok
            else
                "Incorrect operator" |> Error)

    fun input ->
        // We can determine which comparison operator fully matches
        // by checking if any of the operator keywords are left after parsing.
        let operator =
            Array.find
                (fun operatorKeyWord ->
                    match mapper operatorKeyWord input with
                    | Some(_, result) when (Result.isOk result) -> true
                    | _ -> false)
                (RelationalOperator.All())

        // At least some operator should have fully matched at this point
        (mapper operator |> fMap (fun (Ok result) -> result)) input

let parseConditional = parseAlt parseBooleanValue parseConditionalExpression

let parseKeyWordIf = parseKeyWord "if"

let parseKeyWordThen = parseKeyWord "then"

let parseKeyWordElse = parseKeyWord "else"

let rec parseIfThenElse input =
    parseSeq (parseIgnore parseKeyWordIf) (fun _ ->
        parseSeq (parseIgnore (parseChar '(')) (fun _ ->
            parseSeq parseConditional (fun cond ->
                parseSeq (parseIgnore (parseChar ')')) (fun _ ->
                    parseSeq (parseIgnore parseKeyWordThen) (fun _ ->
                        parseSeq (parseIgnore (parseChar '(')) (fun _ ->
                            parseSeq (parseAlt parseIfThenElse parseAdd) (fun trueBranch ->
                                parseSeq (parseIgnore (parseChar ')')) (fun _ ->
                                    parseSeq (parseIgnore parseKeyWordElse) (fun _ ->
                                        parseSeq (parseIgnore (parseChar '(')) (fun _ ->
                                            parseSeq (parseAlt parseIfThenElse parseAdd) (fun elseBranch ->
                                                parseSeq (parseIgnore (parseChar ')')) (fun _ ->
                                                    fMap (fun _ -> IfThenElse(cond, trueBranch, elseBranch)) parseEpsilon)))))))))))) input

let parseAssignment =
    parseSeq parseIdentifier (fun identifierName ->
        parseSeq (parseIgnore (parseChar '=')) (fun _ ->
            fMap (fun expr -> VarAssignment(identifierName, expr)) (parseAlt parseIfThenElse parseAdd)))

let parseProgram =
    parseList (parseAlt parsePrint parseAssignment) (parseIgnore (parseChar '\n'))
