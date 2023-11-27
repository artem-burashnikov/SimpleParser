namespace SimpleParser.Tests

open System.Collections.Generic
open Expecto
open System.IO
open FsCheck
open SimpleParser
open SimpleParser.Combinators
open SimpleParser.Definitions
open SimpleParser.ExprParser
open SimpleParser.Interpreter
open SimpleParser.Generator

module Helper =
    let correctTestDataPath =
        Path.Combine [|
            __SOURCE_DIRECTORY__
            "TestData"
            "CorrectInput"
        |]

    let correctTestFiles = Directory.EnumerateFiles correctTestDataPath

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

    let correctInputFiles = Seq.fold folder (Dictionary<string, string>()) correctTestFiles

    let incorrectInputFiles = Seq.fold folder (Dictionary<string, string>()) incorrectTestFiles

    let makeCodeFromFile file = String.concat "\n" (File.ReadAllLines file)

    let makeAst code =
        match run parseProgram code with
        | None -> failtest $"Incorrect code input. Failed at: {code}"
        | Some(_, ast) -> ast

module TestDataGenerator =

    type ExprGenerator() =
        static member Ast() =

            let getSize = Gen.choose(0, 10) |> Gen.sample 0 1 |> List.head

            let genRelationalOperator = Arb.generate<RelationalOperator>

            let genBoolValue = Gen.elements [| True; False |]

            let genVariableName = Gen.elements [| 'a' .. 'z' |] |> Gen.map string

            let genIdentifier = genVariableName |> Gen.map (fun ch -> Var(ch, Undefined))


            let rec genArithmeticExpression size =

                let genNumber = Gen.choose(1, 100) |> Gen.map Number

                let rec genMultiply depth =
                    match depth with
                    | n when n = 0 -> Gen.oneof [
                        genNumber           |> Gen.map (fun n -> Multiply [n])
                        genIdentifier       |> Gen.map (fun n -> Multiply [n])
                        ]
                    | n when n > 0 -> Gen.listOfLength n (Gen.oneof [genNumber; genIdentifier]) |> Gen.map Multiply
                    | e -> failwith $"genMultiply Depth parameter should be positive. Depth: {e}"

                and genAdd depth =
                    match depth with
                    | n when n = 0 -> (genMultiply 0) |> Gen.map (fun x -> Add [x])
                    | n when n > 0 -> Gen.listOfLength n (genMultiply (n / 2)) |> Gen.map Add
                    | e -> failwith $"genMultiply Depth parameter should be positive. Depth: {e}"

                genAdd size


            and genBooleanExpression size =
                Gen.oneof [
                    genBoolValue
                    Gen.zip3 genRelationalOperator (genArithmeticExpression (size / 2)) (genArithmeticExpression (size / 2)) |> Gen.map Expression
                ]

            and genIfThenElse size =
                match size with
                | n when n = 0 ->
                    Gen.oneof [
                        Gen.zip3 (genBooleanExpression (size / 2)) (genArithmeticExpression (size / 2)) (genArithmeticExpression (size / 2))
                        Gen.zip3 (genBooleanExpression (size / 2)) (genBooleanExpression (size / 2) |> Gen.map BooleanExpr) (genBooleanExpression getSize |> Gen.map BooleanExpr)
                        ]
                        |> Gen.map IfThenElse
                | n when n > 0 ->
                    Gen.oneof [
                        Gen.zip3 (genBooleanExpression (size / 2)) (genArithmeticExpression (size / 2)) (genArithmeticExpression (size / 2))
                        Gen.zip3 (genBooleanExpression (size / 2)) (genBooleanExpression (size / 2) |> Gen.map BooleanExpr) (genBooleanExpression (size / 2) |> Gen.map BooleanExpr)
                        Gen.zip3 (genBooleanExpression (size / 2)) (genIfThenElse (size / 2) |> Gen.map (fun x -> Add [Multiply [x]])) (genIfThenElse (size / 2) |> Gen.map (fun x -> Add [Multiply [x]]))
                        ]
                        |> Gen.map IfThenElse
                | _ -> failwith "Size of ifThenElse must be positive."

            let genAst size =
                Gen.listOfLength size
                    (Gen.oneof [
                        Gen.oneof [
                            genArithmeticExpression size
                            genBooleanExpression    size |> Gen.map BooleanExpr
                            genIfThenElse           size |> Gen.map (fun x -> Add [Multiply [x]])
                        ]
                        |> Gen.map (fun expr -> VarAssignment(genVariableName |> Gen.sample 1 1 |> List.head, expr))

                        Gen.oneof [
                            genArithmeticExpression size
                            genBooleanExpression    size |> Gen.map BooleanExpr
                            genIfThenElse           size |> Gen.map (fun x -> Add [Multiply [x]])
                        ]
                        |> Gen.map Print
                    ])

            gen {
                let! depth = Gen.choose (2, 10)
                return! genAst depth}
            |> Arb.fromGen

    let addToConfig config =
        { config with
            arbitrary =
                typeof<ExprGenerator> :: config.arbitrary }

    let config = addToConfig FsCheckConfig.defaultConfig
    let testCustomProp name = testPropertyWithConfig config name


module ParsingCorrectManualTests =
    open Helper

    [<Tests>]
    let tests =
        testList "AST comparison" [
            testCase "Parsing: identifier"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["identifier"]

                let expectedResult = [
                    VarAssignment ("identifier", Add [Multiply [Number 42]])
                    Print (Add [Multiply [Var ("identifier", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse assignment."

            testCase "Parsing: addition"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["addition"]

                let expectedResult = [
                    VarAssignment ("a", Add [Multiply [Number 5]])
                    VarAssignment ("b", Add [Multiply [Number 7]])
                    VarAssignment ("c", Add [Multiply [Var ("a", Undefined)]; Multiply [Var ("b", Undefined)]])
                    Print         (     Add [Multiply [Var ("b", Undefined)]; Multiply [Var ("a", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse addition."

            testCase "Parsing: multiplication"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["multiplication"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 8]])
                    VarAssignment ("y", Add [Multiply [Number 3]])
                    VarAssignment ("z", Add [Multiply [Var ("x", Undefined); Var ("y", Undefined)]])
                    VarAssignment ("w", Add [Multiply [Var ("y", Undefined); Var ("x", Undefined)]])
                    Print (             Add [Multiply [Var ("w", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse multiplication."

            testCase "Parsing: arithmetic"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["arithmetic"]

                let expectedResult = [
                    VarAssignment ("a", Add [Multiply [Number 3]])
                    VarAssignment ("b", Add [Multiply [Number 5]])
                    VarAssignment ("c", Add [Multiply [Number 2]])
                    VarAssignment ("z", Add [Multiply [Var ("a", Undefined)]; Multiply [Var ("b", Undefined); Var ("c", Undefined)]])
                    Print (             Add [Multiply [Var ("c", Undefined); Var ("b", Undefined)]; Multiply [Var ("a", Undefined)]])]

                Expect.equal actualResult expectedResult "Failed to parse arithmetic."

            testCase "Parsing: arithmeticIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["arithmeticIF"]

                let expectedResult = [
                    VarAssignment ("y", Add [Multiply [Number 5]])
                    VarAssignment ("x", Add [Multiply [Number 10]])
                    Print (             Add [
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

            testCase "Parsing: trueIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["trueIF"]

                let expectedResult = [
                    Print (Add [Multiply [IfThenElse (True,
                                          Add [Multiply [Number 1]],
                                          Add [Multiply [Number 2]])]])]

                Expect.equal actualResult expectedResult "Failed to parse always true if."

            testCase "Parsing: falseIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["falseIF"]

                let expectedResult = [
                    Print (Add [Multiply [IfThenElse (False,
                                          Add [Multiply [Number 1]],
                                          Add [Multiply [Number 2]])]])]

                Expect.equal actualResult expectedResult "Failed to parse always false if."

            testCase "Parsing: nestedIFs"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["nestedIFs"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 100]])
                    VarAssignment ("y", Add [Multiply [Number 1]])
                    Print (             Add [Multiply
                                                     [IfThenElse (Expression (GreaterThan,
                                                                                     Add [Multiply [Var ("x", Undefined)]],
                                                                                     Add [Multiply [Var ("y", Undefined)]]),
                                                                  Add [Multiply [IfThenElse (Expression (LessThan,
                                                                                                                 Add [Multiply [Var ("x", Undefined)]],
                                                                                                                 Add [Multiply [Var ("y", Undefined)]]),
                                                                                              Add [Multiply [Number 10]; Multiply [Var ("y", Undefined)]],
                                                                                              Add [Multiply [Number 2; Number 2]])]],
                                                                  Add [Multiply [Number 1]])]])]

                Expect.equal actualResult expectedResult "Failed to parse nested ifs."

            testCase "Parsing: trueAssignment"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["trueAssignment"]

                let expectedResult = [
                    VarAssignment ("x", BooleanExpr True)
                    Print (Add [Multiply [Var ("x", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "Failed to parse trueAssignment."

            testCase "Parsing: falseAssignment"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["falseAssignment"]

                let expectedResult = [
                    VarAssignment ("x", BooleanExpr False)
                    Print (Add [Multiply [Var ("x", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "Failed to parse falseAssignment."

            testCase "Parsing: boolExpression"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["boolExpression"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 2]])
                    VarAssignment ("y", Add [Multiply [Number 3]])
                    Print (BooleanExpr (Expression (LessThan,
                                                    Add [Multiply [Var ("x", Undefined)]],
                                                    Add [Multiply [Var ("y", Undefined)]])))]

                Expect.equal actualResult expectedResult "Failed to parse boolExpression."

            testCase "Parsing: boolValueFromIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["boolValueFromIF"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [IfThenElse (True,
                                                                   BooleanExpr True,
                                                                   BooleanExpr False)]])
                    Print (             Add [Multiply [IfThenElse (True,
                                                                   BooleanExpr True,
                                                                   BooleanExpr False)]])]

                Expect.equal actualResult expectedResult "Failed to parse boolValueFromIF." ]

module ParsingIncorrectManualTests =
    open Helper

    [<Tests>]
    let tests =
        testList "Deliberate mistakes" [
            testCase "Parsing: invalidAssignment"
            <| fun _ ->
                Expect.throws (fun _ -> makeAst <| makeCodeFromFile incorrectInputFiles["invalidAssignment"] |> ignore) "Invalid assignment has been parsed."

            testCase "Parsing: invalidIdentifier"
            <| fun _ ->
                Expect.throws (fun _ -> makeAst <| makeCodeFromFile incorrectInputFiles["invalidIdentifier"] |> ignore) "Invalid identifier has been parsed."

            testCase "Parsing: invalidRelationalOp"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["invalidRelationalOp"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "InvalidRelationalOp managed to parse correctly. 'if' can't be a Var."

            testCase "Parsing: missingElse"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["missingElse"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "MissingElse expression managed to parse correctly. 'if' can't be a Var."

            testCase "Parsing: missingThen"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["missingThen"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "MissingThen expression managed to parse correctly. 'if' can't be a Var."

            testCase "Parsing: missingPrint"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["missingPrint"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Number 10]])
                ]

                Expect.equal actualResult expectedResult "MissingPrint expression managed to parse correctly. Empty variables should have not been parsed."

            testCase "Parsing: missingConditionalParentheses"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["missingConditionalParentheses"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("iftruethen", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "MissingConditionalParentheses expression managed to parse correctly. 'iftruethen' followed by correct IfClause syntax should have not been parsed."

            testCase "Parsing: statementInBranch"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["statementInBranch"]

                let expectedResult = [
                    VarAssignment ("x", Add [Multiply [Var ("if", Undefined)]])
                ]

                Expect.equal actualResult expectedResult "StatementInBranch expression managed to parse correctly. Statements are not expressions."

            testCase "Parsing: mismatchingTypes"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["mismatchingTypes"]

                let expectedResult = [
                    VarAssignment ("x", BooleanExpr True);
                    VarAssignment ("y", Add [
                                            Multiply [Var ("x", Undefined)]
                                            Multiply [Number 2]])
                    Print (Add [Multiply [Var ("y", Undefined)]])]

                Expect.equal actualResult expectedResult "MismatchingTypes are still parsed correctly."

            testCase "Parsing: complexMismatchingTypes"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile incorrectInputFiles["complexMismatchingTypes"]

                let expectedResult = [
                    VarAssignment ("y", BooleanExpr (Expression (GreaterThan,
                                                                 Add [Multiply [Number 10]],
                                                                 Add [Multiply [Number 3]])))
                    VarAssignment ("x", Add [Multiply [IfThenElse (Expression (LessThan,
                                                                               Add [Multiply [Number 3]],
                                                                               Add [Multiply [Number 5]]),
                                                       Add [Multiply [Var ("y", Undefined); Number 3]],
                                                       BooleanExpr True)]])
                    Print (Add [Multiply [Var ("x", Undefined)]])]

                Expect.equal actualResult expectedResult "complexMismatchingTypes: types are not interfered during parsing. Still has to be parsed correctly." ]


module InterpreterCorrectManualTests =
    open Helper

    [<Tests>]
    let tests =
        testList "AST evaluation" [
            testCase "Interpreter: addition"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["addition"] |> evalProgram
                let expectedResult = 12

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate addition."

            testCase "Interpreter: arithmetic"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["arithmetic"] |> evalProgram
                let expectedResult = 13

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate multiplication."

            testCase "Interpreting: arithmeticIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["arithmeticIF"] |> evalProgram
                let expectedResult = 102

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate arithmeticIF."

            testCase "Interpreter: boolExpression"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["boolExpression"] |> evalProgram
                let expectedResult = true

                Expect.equal actualResult.BoolResult expectedResult "Failed to evaluate bool expression."

            testCase "Interpreter: boolValueFromIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["boolValueFromIF"] |> evalProgram
                let expectedResult = true

                Expect.equal actualResult.BoolResult expectedResult "Failed to evaluate boolValueFromIF."

            testCase "Interpreter: falseAssignment"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["falseAssignment"] |> evalProgram
                let expectedResult = false

                Expect.equal actualResult.BoolResult expectedResult "Failed to evaluate falseAssignment."

            testCase "Interpreter: falseIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["falseIF"] |> evalProgram
                let expectedResult = 2

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate falseIF."

            testCase "Interpreter: identifier"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["identifier"] |> evalProgram
                let expectedResult = 42

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate identifier."

            testCase "Interpreter: multiplication"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["multiplication"] |> evalProgram
                let expectedResult = 24

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate identifier."

            testCase "Interpreter: nestedIFs"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["nestedIFs"] |> evalProgram
                let expectedResult = 4

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate nestedIFs."

            testCase "Interpreter: trueAssignment"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["trueAssignment"] |> evalProgram
                let expectedResult = true

                Expect.equal actualResult.BoolResult expectedResult "Failed to evaluate trueAssignment."

            testCase "Interpreter: trueIF"
            <| fun _ ->
                let actualResult = makeAst <| makeCodeFromFile correctInputFiles["trueIF"] |> evalProgram
                let expectedResult = 1

                Expect.equal actualResult.IntResult expectedResult "Failed to evaluate trueIF." ]


module InterpreterIncorrectManualTests =
    open Helper

    [<Tests>]
    let tests =
        testList "Type check" [
            testCase "Interpreter: mismatchingTypes"
            <| fun _ ->
                Expect.throws (fun _ -> makeAst <| makeCodeFromFile incorrectInputFiles["mismatchingTypes"] |> evalProgram |> ignore) "Addition of boolean and integer should have not been evaluated."

            testCase "Interpreter: complexMismatchingTypes"
            <| fun _ ->
                Expect.throws (fun _ -> makeAst <| makeCodeFromFile incorrectInputFiles["complexMismatchingTypes"] |> evalProgram |> ignore) "Multiplication of boolean and integer should have not been evaluated." ]


module Generator =
     open Helper
     open Analyzer
     open TestDataGenerator

     [<Tests>]
     let tests =
         testList "AST invariant" [
             testCase "Generator x Parsing"
             <| fun _ ->

                 let ctx = Context(Undefined, Dictionary<string, VarType * obj>())
                 let allFiles = Seq.fold (fun result x -> x :: result) [] correctTestFiles
                 let allAST = Seq.map (fun filePath -> makeAst <| makeCodeFromFile filePath) allFiles |> Seq.toList |> List.map (optimize ctx)
                 let allActualResults =
                     List.map generateCode allAST
                     |> List.map (fun input ->
                     match run parseProgram input with
                     | Some(_, res) -> optimize ctx res
                     | None -> failwith "Couldn't parse input"
                     )

                 Expect.sequenceEqual allActualResults allAST ""

             testCustomProp "Auto generator x parsing"
             <| fun (program: SourceAst list) ->
                 let programInvariant = generateCode program |> makeAst
                 Expect.sequenceEqual programInvariant program "" ]
