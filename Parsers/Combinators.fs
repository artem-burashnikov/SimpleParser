module Parsers.Combinators

type Parser<'Result> = char list -> Option<List<char> * 'Result>

let satisfy predicate : Parser<char> =
    fun input ->
        match input with
        | hd :: tl when predicate hd -> Some(tl, hd)
        | _ -> None

let parseChar ch : Parser<char> = satisfy (fun x -> ch = x)

let parseEpsilon: Parser<unit> = fun input -> Some(input, ())

let parseSeq (parser1: Parser<'A>) (continuation: 'A -> Parser<'B>) : Parser<'B> =
    fun input ->
        match parser1 input with
        | None -> None
        | Some(charList, result) -> continuation result charList

let parseAlt (parser1: Parser<'A>) (parser2: Parser<'A>) : Parser<'A> =
    fun input ->
        match parser1 input with
        | None -> parser2 input
        | parsedInput -> parsedInput

let fMap (fnc: 'R1 -> 'R2) (parser: Parser<'R1>) : Parser<'R2> =
    fun input ->
        match parser input with
        | None -> None
        | Some(charList, result) -> Some(charList, fnc result)

let rec parseMany (parser: Parser<'A>) : Parser<List<'A>> =
    parseAlt
        (parseSeq parser (fun result -> fMap (fun tl -> result :: tl) (parseMany parser)))
        (fMap (fun _ -> []) parseEpsilon)

let parseSome (parser: Parser<'A>) : Parser<List<'A>> =
    parseSeq parser (fun result -> fMap (fun tl -> result :: tl) (parseMany parser))

let parseList (elementParser: Parser<'A>) (parseSeparator: Parser<unit>) =
    parseSeq elementParser (fun result ->
        (parseMany (parseSeq parseSeparator (fun _ -> elementParser)))
        |> fMap (fun tl -> result :: tl))

let parseIgnore parser = fMap ignore parser

let parseKeyWord (kw: string) : Parser<string> =
    let chars = kw.ToCharArray()

    Array.fold
        (fun parser currChar -> parseSeq parser (fun result -> fMap (fun char -> char :: result) (parseChar currChar)))
        (fMap (fun _ -> []) parseEpsilon)
        chars
    |> fMap (fun charList -> charList |> List.rev |> Array.ofList |> System.String)

let run =
    fun (parser: Parser<'R>) (input: string) ->
        let listOfChars = input.ToCharArray() |> List.ofArray
        parser listOfChars
