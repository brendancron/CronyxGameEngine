
open Cronyx.Evaluation.Models.Expressions
open Cronyx.Evaluation.Components.Parser
open Cronyx.Evaluation.Components.Lexer
open Cronyx.Evaluation.Components.Inference

let source = "var id = (x) -> x; id 5.4"
let tokens = scan source
printfn "%A" tokens
let expr   = parse tokens
printfn "%A" expr
inferType Map.empty expr |> printfn "%A"

