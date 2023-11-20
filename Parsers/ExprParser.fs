module Parsers.ExprParser

open Parsers.Combinators
open Parsers.AST
open Parsers.Helper

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

let parseAssignment =
    parseSeq parseIdentifier (fun identifierName ->
        parseSeq (parseIgnore (parseChar '=')) (fun _ ->
            fMap (fun expr -> VarAssignment(identifierName, expr)) parseAdd))

let parseKeyWordPrint = parseKeyWord "print"

let parsePrint =
    parseSeq parseKeyWordPrint (fun _ -> parseSeq (parseChar ':') (fun _ -> fMap Print parseAdd))

let parseBooleanTrue =
    parseSeq (parseIgnore (parseKeyWord "true")) (fun _ -> fMap (fun _ -> True) parseEpsilon)

let parseBooleanFalse =
    parseSeq (parseIgnore (parseKeyWord "false")) (fun _ -> fMap (fun _ -> False) parseEpsilon)

let parseBooleanValue = parseAlt parseBooleanTrue parseBooleanFalse

let mapper =
    fun relationalOperator ->
        parseList parseAdd (parseIgnore (parseKeyWord relationalOperator))
        |> fMap (fun sourceExprList ->
            if List.length sourceExprList = 2 then
                printfn $"%A{sourceExprList}"
                System.Console.ReadKey() |> ignore
                Ok <| BooleanExpression(parseKeyWrdToRelationalOp relationalOperator, sourceExprList[0], sourceExprList[1])
            else
                Error "Incorrect operator")

let parseConditionalExpression =
    fun input ->
        // Find which comparison operator fully matches
        let operator =
            Array.find (fun operatorKeyWord ->
            match mapper operatorKeyWord input with
            | Some(_, result) when (Result.isOk result) -> true
            | _ -> false
            ) relationalOperators
        
        // At least some operator should have fully matched at this point
        (mapper operator |> fMap (fun (Ok result) -> result)) input
            
let parseConditional =
    parseAlt parseBooleanValue parseConditionalExpression

let parseKeyWordIf = parseKeyWord "if"

let parseKeyWordThen = parseKeyWord "then"

let parseKeyWordElse = parseKeyWord "else"

let rec parseIfThenElse input =
    parseSeq (parseIgnore parseKeyWordIf) (fun _ ->
        parseSeq parseConditionalExpression (fun cond ->
            parseSeq (parseIgnore parseKeyWordThen) (fun _ ->
                parseSeq (parseAlt parseAdd parseIfThenElse) (fun trueBranch ->
                    parseSeq (parseIgnore parseKeyWordElse) (fun _ ->
                        parseSeq (parseAlt parseAdd parseIfThenElse) (fun elseBranch ->
                            parseSeq parseEpsilon (fun _ ->
                                fMap (fun _ -> IfThenElse(cond, trueBranch, elseBranch)) parseEpsilon))))))) input

let parseProgram =
    parseList (parseAlt parsePrint parseAssignment) (parseIgnore (parseChar '\n'))
