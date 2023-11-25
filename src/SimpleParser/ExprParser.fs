module SimpleParser.ExprParser

open Microsoft.FSharp.Core
open SimpleParser.Combinators
open SimpleParser.Definitions

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

let parseLessThanOrEqual =
    parseSeq (parseIgnore (parseKeyWord "<=")) (fun _ -> fMap (fun _ -> LessThanOrEqual) parseEpsilon)

let parseNotEqual =
    parseSeq (parseIgnore (parseKeyWord "<>")) (fun _ -> fMap (fun _ -> NotEqual) parseEpsilon)

let parseGreaterThanOrEqual =
    parseSeq (parseIgnore (parseKeyWord "<>")) (fun _ -> fMap (fun _ -> GreaterThanOrEqual) parseEpsilon)

let parseEqual =
    parseSeq (parseIgnore (parseKeyWord "=")) (fun _ -> fMap (fun _ -> Equal) parseEpsilon)

let parseGreaterThan =
    parseSeq (parseIgnore (parseKeyWord ">")) (fun _ -> fMap (fun _ -> GreaterThan) parseEpsilon)

let parseLessThen =
    parseSeq (parseIgnore (parseKeyWord "<")) (fun _ -> fMap (fun _ -> LessThan) parseEpsilon)

let parseKeyWordIf = parseKeyWord "if"

let parseKeyWordThen = parseKeyWord "then"

let parseKeyWordElse = parseKeyWord "else"

let rec parseMultiply input =
    let parser = parseAltCombine [parseIfThenElse; parseNumber; fMap (fun varName -> Var(varName, Integer)) parseIdentifier]

    (parseList parser (parseIgnore (parseChar '*')) |> fMap Multiply) input

and parseAdd input =
    (parseList parseMultiply (parseIgnore (parseChar '+')) |> fMap Add) input

and parseConditionalExpression input =
    let parseOp = parseAltCombine [
        parseLessThanOrEqual
        parseNotEqual
        parseGreaterThanOrEqual
        parseEqual
        parseGreaterThan
        parseLessThen
    ]

    parseSeq parseAdd (fun lhs ->
        parseSeq parseOp (fun op ->
            parseAdd |> fMap (fun rhs -> Expression(op, lhs, rhs)))) input

and parseConditional input =
    parseAltCombine [parseBooleanTrue; parseBooleanFalse; parseConditionalExpression] input

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
                                                |> fMap (fun _ -> IfThenElse(cond, trueBranch, elseBranch))))))))))))) input

and parseAssignment input =
    parseSeq parseIdentifier (fun identifierName ->
        parseSeq (parseIgnore (parseChar '=')) (fun _ ->
            parseAdd
            |> fMap (fun expr -> VarAssignment(identifierName, expr)))) input

let parseKeyWordPrint = parseKeyWord "print"

let parsePrint =
    parseSeq parseKeyWordPrint (fun _ -> parseSeq (parseChar ':') (fun _ -> fMap Print parseAdd))

let parseProgram =
    parseList (parseAlt parseAssignment parsePrint) (parseIgnore (parseChar '\n'))
