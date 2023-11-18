open Parsers.Combinators
open Parsers.Interpreter

let res =
    run
        Parsers.ExprParser.parseProgram
        "x=2*10+2*3*4+20
var=x+x
print:var*2
print=var*x
print:print"

match res with
| None -> printfn "Incorrect input"
| Some(_, ast) ->
    evalProgram ast
