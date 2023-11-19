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

let parseKeyWordPrint = parseKeyWord "print"

let parsePrint =
    parseSeq parseKeyWordPrint (fun _ -> parseSeq (parseChar ':') (fun _ -> fMap Print parseAdd))

let parseBooleanTrue =
    parseSeq (parseIgnore (parseKeyWord "true")) (fun _ -> fMap (fun _ -> True) parseEpsilon)

let parseBooleanFalse =
    parseSeq (parseIgnore (parseKeyWord "false")) (fun _ -> fMap (fun _ -> False) parseEpsilon)

let parseBooleanValue = parseAlt parseBooleanTrue parseBooleanFalse

let parseLessThanOrEqual =
    parseSeq (parseIgnore (parseKeyWord "<=")) (fun _ -> fMap (fun _ -> LessThanOrEqual) parseEpsilon)

let parseLessThan =
    parseSeq (parseIgnore (parseChar '<')) (fun _ -> fMap (fun _ -> LessThan) parseEpsilon)

let parseNotEqual =
    parseSeq (parseIgnore (parseKeyWord "<>")) (fun _ -> fMap (fun _ -> NotEqual) parseEpsilon)

let parseEqual =
    parseSeq (parseIgnore (parseChar '=')) (fun _ -> fMap (fun _ -> Equal) parseEpsilon)

let parseGreaterThan =
    parseSeq (parseIgnore (parseKeyWord "<=")) (fun _ -> fMap (fun _ -> GreaterThan) parseEpsilon)

let parseGreaterThanOrEqual =
    parseSeq (parseIgnore (parseKeyWord "<=")) (fun _ -> fMap (fun _ -> GreaterThanOrEqual) parseEpsilon)

let parseBooleanOperator =
    parseAlt
        parseGreaterThanOrEqual
        (parseAlt
            parseGreaterThan
            (parseAlt
                parseEqual
                    (parseAlt
                        parseNotEqual
                            (parseAlt
                                parseLessThan
                                parseLessThanOrEqual))))

let parseKeyWordIf = parseKeyWord "if"

let parseKeyWordThen = parseKeyWord "then"

let parseKeyWordElse = parseKeyWord "else"

let parseProgram =
    parseList (parseAlt parsePrint parseAssignment) (parseIgnore (parseChar '\n'))
