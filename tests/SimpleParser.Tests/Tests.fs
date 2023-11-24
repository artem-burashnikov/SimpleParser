namespace SimpleParser.Tests

open System.Collections.Generic
open Expecto
open System.IO
open SimpleParser.Combinators
open SimpleParser.AST
open SimpleParser.ExprParser

module ManualTests =

    let correctTestDataPath =
        Path.Combine [|
            __SOURCE_DIRECTORY__
            "TestData"
            "CorrectInput"
        |]

    let correctTestFiles = Directory.EnumerateFiles correctTestDataPath

    let folder (state: Dictionary<_, _>) (value: string) =
        state.Add((Path.GetFileNameWithoutExtension value), value)
        state

    let correctInputFiles = Seq.fold folder (Dictionary<string, string>()) correctTestFiles

    let makeAST file =
        let programCode =
            String.concat "\n" (File.ReadAllLines file)

        match run parseProgram programCode with
        | None -> failtest "incorrect code input"
        | Some(_, ast) -> ast


    [<Tests>]
    let tests =
        testList "AST comparison" [
            testCase "identifier"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["identifier"]

                let expectedResult = [
                    VarAssignment ("identifier", Add [Multiply [Number 42]])]

                Expect.equal actualResult expectedResult "Failed to parse assignment"

            testCase "addition"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["addition"]

                let expectedResult = [
                    VarAssignment ("a", Add [Multiply [Number 5]])
                    VarAssignment ("b", Add [Multiply [Number 7]])
                    VarAssignment ("c", Add [Multiply [Var "a"]; Multiply [Var "b"]])
                    Print         (     Add [Multiply [Var "b"]; Multiply [Var "a"]])]

                Expect.equal actualResult expectedResult "Failed to parse addition"

            testCase "multiplication"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["multiplication"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 8]])
                    VarAssignment ("y", Add [Multiply [Number 3]])
                    VarAssignment ("z", Add [Multiply [Var "x"; Var "y"]])
                    VarAssignment ("w", Add [Multiply [Var "y"; Var "x"]])]

                Expect.equal actualResult expectedResult "Failed to parse multiplication"

            testCase "arithmetic"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["arithmetic"]

                let expectedResult = [
                    VarAssignment ("a", Add [Multiply [Number 3]])
                    VarAssignment ("b", Add [Multiply [Number 5]])
                    VarAssignment ("c", Add [Multiply [Number 2]])
                    VarAssignment ("z", Add [Multiply [Var "a"]; Multiply [Var "b"; Var "c"]])
                    VarAssignment ("w", Add [Multiply [Var "c"; Var "b"]; Multiply [Var "a"]])]

                Expect.equal actualResult expectedResult "Failed to parse arithmetic"

            testCase "arithmeticIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["arithmeticIF"]

                let expectedResult = [
                    VarAssignment ("y", Add [Multiply [Number 5]])
                    VarAssignment ("x", Add [Multiply [Number 10]])
                    VarAssignment ("z", Add [
                                             Multiply [Var "x"; Var "y"]
                                             Multiply [IfThenElse (BooleanExpression (GreaterThan,
                                                                                      Add [Multiply [Var "x"]],
                                                                                      Add [Multiply [Number 10]]),
                                                                   Add [Multiply [Number 1]],
                                                                   Add [Multiply [Number 2]])]
                                             Multiply [
                                                       Var "x"
                                                       IfThenElse (BooleanExpression (LessThan,
                                                                                      Add [Multiply [Var "y"]],
                                                                                      Add [Multiply [Number 20]]),
                                                                   Add [Multiply [Number 1]],
                                                                   Add [Multiply [Number 2]])
                                                       Var "y"]])]

                Expect.equal actualResult expectedResult "Failed to parse arithmetic with if"

            testCase "trueIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["trueIF"]

                let expectedResult = [
                    VarAssignment ("result", Add [Multiply [IfThenElse (True,
                                                                        Add [Multiply [Number 1]],
                                                                        Add [Multiply [Number 2]])]])]

                Expect.equal actualResult expectedResult "Failed to parse always true if"

            testCase "falseIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["falseIF"]

                let expectedResult = [
                    VarAssignment ("result", Add [Multiply [IfThenElse (False,
                                                                         Add [Multiply [Var "x"]],
                                                                         Add [Multiply [Number 2]])]])]

                Expect.equal actualResult expectedResult "Failed to parse always false if"

            testCase "nestedIFs"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["nestedIFs"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 100]])
                    VarAssignment ("y", Add [Multiply [Number 1]])
                    VarAssignment ("r", Add [Multiply
                                                     [IfThenElse (BooleanExpression (GreaterThan,
                                                                                     Add [Multiply [Var "x"]],
                                                                                     Add [Multiply [Var "y"]]),
                                                                  Add [Multiply [IfThenElse (BooleanExpression (LessThan,
                                                                                                                 Add [Multiply [Var "x"]],
                                                                                                                 Add [Multiply [Var "y"]]),
                                                                                              Add [Multiply [Number 10]; Multiply [Var "r"]],
                                                                                              Add [Multiply [Number 2; Number 2]])]],
                                                                  Add [Multiply [Number 1]])]])]

                Expect.equal actualResult expectedResult "Failed to parse nested ifs"
        ]
