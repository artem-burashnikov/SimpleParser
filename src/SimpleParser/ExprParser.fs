module SimpleParser.ExprParser

open Microsoft.FSharp.Core
open SimpleParser.Combinators
open SimpleParser.AST

let isOk =
    function
    | Ok _ -> true
    | Error _ -> false

let parseIdentifier: Parser<string> =
    parseSome (satisfy (fun x -> List.contains x [ 'a' .. 'z' ]))
    |> fMap (fun res -> res |> Array.ofList |> System.String)

let parseNumber =
    parseSeq (satisfy (fun ch -> List.contains ch [ '1' .. '9' ])) (fun parsedChar ->
        (parseMany (satisfy (fun x -> List.contains x [ '0' .. '9' ]))) |> fMap (fun restOfInput -> parsedChar :: restOfInput))
    |> fMap (fun charList -> charList |> Array.ofList |> System.String |> int |> Number)

let parseBooleanTrue =
    parseSeq (parseIgnore (parseKeyWord "true")) (fun _ -> fMap (fun _ -> True) parseEpsilon)

let parseBooleanFalse =
    parseSeq (parseIgnore (parseKeyWord "false")) (fun _ -> fMap (fun _ -> False) parseEpsilon)

let parseBooleanValue = parseAlt parseBooleanTrue parseBooleanFalse

let parseKeyWordIf = parseKeyWord "if"

let parseKeyWordThen = parseKeyWord "then"

let parseKeyWordElse = parseKeyWord "else"

let rec parseMultiply input =
    let alt1 = parseIfThenElse
    let alt2 = parseAlt parseNumber (fMap Var parseIdentifier)

    (parseList (parseAlt alt1 alt2) (parseIgnore (parseChar '*'))
     |> fMap Multiply)
    <| input

and parseAdd input =
    (parseList parseMultiply (parseIgnore (parseChar '+'))
     |> fMap Add)
    <| input

and parseConditionalExpression input =
    let mapper relationalOperator =
        parseList parseAdd (parseIgnore (parseKeyWord relationalOperator))
        |> fMap (
            function
            | [ lhs; rhs ] -> Ok (BooleanExpression(RelationalOperator.FromString relationalOperator, lhs, rhs))
            | _ -> Error $"Failed to parse comparison operator: {relationalOperator}")

    let operator =
        Array.find
            <| fun operatorKeyWord ->
                match mapper operatorKeyWord input with
                | Some(_, result) when isOk result -> true
                | _ -> false
            <| RelationalOperator.All()

    mapper operator
     |> fMap
        (function
         | Ok result -> result
         | Error message -> failwith message)
    <| input

and parseConditional input =
    parseAlt parseBooleanValue parseConditionalExpression
    <| input

and parseIfThenElse input =
    parseSeq parseKeyWordIf (fun _ ->
        parseSeq (parseChar '(') (fun _ ->
            parseSeq parseConditional (fun cond ->
                parseSeq (parseChar ')') (fun _ ->
                    parseSeq parseKeyWordThen (fun _ ->
                        parseSeq (parseChar '(') (fun _ ->
                            parseSeq parseAdd (fun trueBranch ->
                                parseSeq (parseChar ')') (fun _ ->
                                    parseSeq parseKeyWordElse (fun _ ->
                                        parseSeq (parseChar '(') (fun _ ->
                                            parseSeq parseAdd (fun elseBranch ->
                                                (parseChar ')')
                                                |> fMap (fun _ -> IfThenElse(cond, trueBranch, elseBranch)))))))))))))
    <| input

and parseAssignment input =
    parseSeq parseIdentifier (fun identifierName ->
        parseSeq (parseIgnore (parseChar '=')) (fun _ ->
            parseAdd
            |> fMap (fun expr -> VarAssignment(identifierName, expr))))
    <| input

let parseKeyWordPrint = parseKeyWord "print"

let parsePrint =
    parseSeq parseKeyWordPrint (fun _ -> parseSeq (parseChar ':') (fun _ -> fMap Print parseAdd))

let parseProgram =
    parseList (parseAlt parseAssignment parsePrint) (parseIgnore (parseChar '\n'))
