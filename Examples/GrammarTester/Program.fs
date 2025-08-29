open Cronyx.Grammar

let myexpr = (TernaryExpr((BoolExpr true), (IntExpr 7), (IntExpr 6)))

let inline addIntFloat i f = float(i) + f

let divExpr = DivExpr (FloatExpr(2.0), FloatExpr(0.0), (/))

// For more information see https://aka.ms/fsharp-console-apps
printfn "%A" (eval divExpr)

// Lambda expression: x ↦ x + 1
let incrExpr =
    LambdaExpr<int,int>(fun x -> x + 1) :> IExpr<int -> int>

// Argument expression: literal 5
let argExpr =
    IntExpr 5 :> IExpr<int>

// Apply the lambda to the argument: (x+1)(5)
let applied =
    ApplyExpr(incrExpr, argExpr) :> IExpr<int>

// Evaluate
printfn "%d" (applied.Eval())   // prints 6