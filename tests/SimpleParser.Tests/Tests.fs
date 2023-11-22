namespace SimpleParser.Tests

open Expecto

module ManualTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "ABC"
            <| fun _ ->
                let actualResult = true
                let expectedResult = true
                Expect.equal actualResult expectedResult "Not an absolute unit"
        ]
