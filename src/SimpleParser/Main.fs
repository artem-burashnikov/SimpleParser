namespace SimpleParser

open System.IO
open Argu
open SimpleParser.Combinators
open SimpleParser.Interpreter

type CliArguments =
    | [<Unique; Mandatory; MainCommand>] FilePath of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | FilePath _ -> "specify a path to the file"

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
                match results.TryGetResult(FilePath) with
                | Some path when
                    (File.Exists path)
                    && (Path.GetExtension path = ".txt")
                    ->
                    path
                | _ ->
                    eprintfn "incorrect file path"
                    exit 1

            let programCode =
                File.ReadAllText filePath
                |> (fun x -> x.Trim())

            let result = run ExprParser.parseProgram programCode

            match result with
            | None ->
                eprintfn "incorrect code input"
                exit 1
            | Some(_, ast) -> evalProgram ast

        run ()
        |> ignore

        0
