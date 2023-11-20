open Parsers.AST
open Parsers.Combinators
open Parsers.Interpreter

let res = run Parsers.ExprParser.parseProgram "x=if(10=3)then(105)else(3)
print:x"

match res with
| None -> printfn "Incorrect input"
| Some(_, ast) ->
    printfn $"%A{res}"
    evalProgram ast
