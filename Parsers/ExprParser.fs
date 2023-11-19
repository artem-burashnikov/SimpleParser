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

let parseAssignment =
    parseSeq parseIdentifier (fun identifierName ->
        parseSeq (parseIgnore (parseChar '=')) (fun _ ->
            fMap (fun expr -> VarAssignment(identifierName, expr)) parseAdd))

let parseKeWordPrint = parseKeyWord "print"

let parsePrint =
    parseSeq parseKeWordPrint (fun _ -> parseSeq (parseChar ':') (fun _ -> fMap Print parseAdd))

let parseProgram =
    parseList (parseAlt parsePrint parseAssignment) (parseIgnore (parseChar '\n'))
