
open Cronyx.Evaluation.Models.Expressions
open Cronyx.Evaluation.Components.Parser
open Cronyx.Evaluation.Components.Lexer
open Cronyx.Evaluation.Components.Inference

let source = "3.2 + 4.5 * 5.1 / 6.0 - 7.3"
let tokens = scan source
printfn "%A" tokens
let expr = parse tokens
match expr with
| Error e -> printfn "Error: %s" e
| Ok expr ->
    printfn "%A" expr
    inferType Map.empty expr |> printfn "%A"

