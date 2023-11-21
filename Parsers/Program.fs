open System.IO
open Argu
open Parsers.Combinators
open Parsers.Interpreter

type CliArguments =
    | [<Unique; Mandatory; AltCommandLine("-i")>] TextFile of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | TextFile _ -> "specify a path to the file"

module Main =

    [<EntryPoint>]
    let main (argv: string array) =

        let errorHandler =
            ProcessExiter(
                colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some System.ConsoleColor.Blue
            )

        let parser = ArgumentParser.Create<CliArguments>(errorHandler = errorHandler)

        let run () =

            let results = parser.ParseCommandLine argv

            let filePath =
                match results.TryGetResult(TextFile) with
                | Some path when (File.Exists path) && (Path.GetExtension path = ".txt") -> path
                | _ ->
                    eprintfn "incorrect file path"
                    exit 1

            let programCode = File.ReadAllText filePath |> (fun x -> x.Trim())

            let result = run Parsers.ExprParser.parseProgram programCode

            match result with
            | None ->
                eprintfn "incorrect code input"
                exit 1
            | Some(_, ast) -> evalProgram ast

        run ()

        0
