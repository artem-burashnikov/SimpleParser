namespace SimpleParser.Tests

open System.Collections.Generic
open Expecto
open System.IO
open SimpleParser.Combinators
open SimpleParser.Definitions
open SimpleParser.ExprParser

module CorrectManualTests =

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

                Expect.equal actualResult expectedResult "Failed to parse assignment."

            testCase "addition"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["addition"]

                let expectedResult = [
                    VarAssignment ("a", Add [Multiply [Number 5]])
                    VarAssignment ("b", Add [Multiply [Number 7]])
                    VarAssignment ("c", Add [Multiply [Var ("a", Undefined)]; Multiply [Var ("b", Undefined)]])
                    Print         (     Add [Multiply [Var ("b", Undefined)]; Multiply [Var ("a", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse addition."

            testCase "multiplication"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["multiplication"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 8]])
                    VarAssignment ("y", Add [Multiply [Number 3]])
                    VarAssignment ("z", Add [Multiply [Var ("x", Undefined); Var ("y", Undefined)]])
                    VarAssignment ("w", Add [Multiply [Var ("y", Undefined); Var ("x", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse multiplication."

            testCase "arithmetic"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["arithmetic"]

                let expectedResult = [
                    VarAssignment ("a", Add [Multiply [Number 3]])
                    VarAssignment ("b", Add [Multiply [Number 5]])
                    VarAssignment ("c", Add [Multiply [Number 2]])
                    VarAssignment ("z", Add [Multiply [Var ("a", Undefined)]; Multiply [Var ("b", Undefined); Var ("c", Undefined)]])
                    VarAssignment ("w", Add [Multiply [Var ("c", Undefined); Var ("b", Undefined)]; Multiply [Var ("a", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse arithmetic."

            testCase "arithmeticIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["arithmeticIF"]

                let expectedResult = [
                    VarAssignment ("y", Add [Multiply [Number 5]])
                    VarAssignment ("x", Add [Multiply [Number 10]])
                    VarAssignment ("z", Add [
                                             Multiply [Var ("x", Undefined); Var ("y", Undefined)]
                                             Multiply [IfThenElse (Expression (GreaterThan,
                                                                                      Add [Multiply [Var ("x", Undefined)]],
                                                                                      Add [Multiply [Number 10]]),
                                                                   Add [Multiply [Number 1]],
                                                                   Add [Multiply [Number 2]])]
                                             Multiply [
                                                       Var ("x", Undefined)
                                                       IfThenElse (Expression (LessThan,
                                                                                      Add [Multiply [Var ("y", Undefined)]],
                                                                                      Add [Multiply [Number 20]]),
                                                                   Add [Multiply [Number 1]],
                                                                   Add [Multiply [Number 2]])
                                                       Var ("y", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse arithmetic with if."

            testCase "trueIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["trueIF"]

                let expectedResult = [
                    VarAssignment ("result", Add [Multiply [IfThenElse (True,
                                                                        Add [Multiply [Number 1]],
                                                                        Add [Multiply [Number 2]])]])]

                Expect.equal actualResult expectedResult "Failed to parse always true if."

            testCase "falseIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["falseIF"]

                let expectedResult = [
                    VarAssignment ("result", Add [Multiply [IfThenElse (False,
                                                                         Add [Multiply [Var ("x", Undefined)]],
                                                                         Add [Multiply [Number 2]])]])]

                Expect.equal actualResult expectedResult "Failed to parse always false if."

            testCase "nestedIFs"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["nestedIFs"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 100]])
                    VarAssignment ("y", Add [Multiply [Number 1]])
                    VarAssignment ("r", Add [Multiply
                                                     [IfThenElse (Expression (GreaterThan,
                                                                                     Add [Multiply [Var ("x", Undefined)]],
                                                                                     Add [Multiply [Var ("y", Undefined)]]),
                                                                  Add [Multiply [IfThenElse (Expression (LessThan,
                                                                                                                 Add [Multiply [Var ("x", Undefined)]],
                                                                                                                 Add [Multiply [Var ("y", Undefined)]]),
                                                                                              Add [Multiply [Number 10]; Multiply [Var ("r", Undefined)]],
                                                                                              Add [Multiply [Number 2; Number 2]])]],
                                                                  Add [Multiply [Number 1]])]])]

                Expect.equal actualResult expectedResult "Failed to parse nested ifs."

            testCase "trueAssignment"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["trueAssignment"]

                let expectedResult = [
                    VarAssignment ("x", BooleanExpr True)
                    Print (Add [Multiply [Var ("x", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "Failed to parse trueAssignment."

            testCase "falseAssignment"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["falseAssignment"]

                let expectedResult = [
                    VarAssignment ("x", BooleanExpr False)
                    Print (Add [Multiply [Var ("x", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "Failed to parse falseAssignment."

            testCase "boolExpression"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["boolExpression"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 2]])
                    VarAssignment ("y", Add [Multiply [Number 3]])
                    Print (BooleanExpr (Expression (LessThan,
                                                    Add [Multiply [Var ("x", Undefined)]],
                                                    Add [Multiply [Var ("y", Undefined)]])))]

                Expect.equal actualResult expectedResult "Failed to parse boolExpression."


            testCase "boolValueFromIF"
            <| fun _ ->
                let actualResult = makeAST correctInputFiles["boolValueFromIF"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [IfThenElse (True,
                                                                   BooleanExpr True,
                                                                   BooleanExpr False)]])
                    Print (             Add [Multiply [IfThenElse (True,
                                                                   BooleanExpr True,
                                                                   BooleanExpr False)]])]

                Expect.equal actualResult expectedResult "Failed to parse boolValueFromIF." ]

module IncorrectManualTests =

    let incorrectTestDataPath =
        Path.Combine [|
            __SOURCE_DIRECTORY__
            "TestData"
            "IncorrectInput"
        |]

    let incorrectTestFiles = Directory.EnumerateFiles incorrectTestDataPath

    let folder (state: Dictionary<_, _>) (value: string) =
        state.Add((Path.GetFileNameWithoutExtension value), value)
        state

    let incorrectInputFiles = Seq.fold folder (Dictionary<string, string>()) incorrectTestFiles

    let makeAST file =
        let programCode =
            String.concat "\n" (File.ReadAllLines file)

        match run parseProgram programCode with
        | None -> failtest "incorrect code input"
        | Some(_, ast) -> ast


    [<Tests>]
    let tests =
        testList "Deliberate mistakes" [
            testCase "invalidAssignment"
            <| fun _ ->
                Expect.throws (fun _ -> makeAST incorrectInputFiles["invalidAssignment"] |> ignore) "Invalid assignment has been parsed."

            testCase "invalidIdentifier"
            <| fun _ ->
                Expect.throws (fun _ -> makeAST incorrectInputFiles["invalidIdentifier"] |> ignore) "Invalid identifier has been parsed."

            testCase "invalidRelationalOp"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["invalidRelationalOp"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "InvalidRelationalOp managed to parse correctly. 'if' can't be a Var."

            testCase "missingElse"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["missingElse"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "MissingElse expression managed to parse correctly. 'if' can't be a Var."

            testCase "missingThen"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["missingThen"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "MissingThen expression managed to parse correctly. 'if' can't be a Var."

            testCase "missingPrint"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["missingPrint"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 10]])
                ]

                Expect.equal actualResult expectedResult "MissingPrint expression managed to parse correctly. Empty variables should have not been parsed."

            testCase "missingConditionalParentheses"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["missingConditionalParentheses"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("iftruethen", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "MissingConditionalParentheses expression managed to parse correctly. 'iftruethen' followed by correct IfClause syntax should have not been parsed."

            testCase "statementInBranch"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["statementInBranch"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "StatementInBranch expression managed to parse correctly. Statements are not expressions."

            testCase "mismatchingTypes"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["mismatchingTypes"]

                let expectedResult = [
                    VarAssignment ("x", BooleanExpr True);
                    VarAssignment ("y", Add [
                                            Multiply [Var ("x", Undefined)]
                                            Multiply [Number 2]])]

                Expect.equal actualResult expectedResult "mismatchingTypes."

            testCase "complexMismatchingTypes"
            <| fun _ ->
                let actualResult = makeAST incorrectInputFiles["complexMismatchingTypes"]

                let expectedResult = [
                    VarAssignment ("y", BooleanExpr (Expression (GreaterThan,
                                                                 Add [Multiply [Number 10]],
                                                                 Add [Multiply [Number 3]])))
                    VarAssignment ("x", Add [Multiply [IfThenElse (Expression (LessThan,
                                                                               Add [Multiply [Number 3]],
                                                                               Add [Multiply [Number 5]]),
                                                       BooleanExpr (Expression (LessThan,
                                                                                Add [Multiply [Var ("y", Undefined)]],
                                                                                Add [Multiply [Number 3]])),
                                                       BooleanExpr True)]])
                    Print (Add [Multiply [Var ("x", Undefined)]])]


                Expect.equal actualResult expectedResult "mismatchingTypes." ]
