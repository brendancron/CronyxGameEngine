open Cronyx.Parsing.Lexer
open Cronyx.Parsing.Parser
open Cronyx.Evaluation.Grammar
open Cronyx.Evaluation.Environment

let source = "print(3 + 4 * 5 + 8);"

let tokens = scan source

printfn "Tokens: %A" (tokens |> List.map (fun t -> t.TokenType, t.Lexeme))

let stmt, remaining, logs = parse<int,int,int> tokens

printfn "Expr: %A" stmt
printfn "Remaining Tokens: %A" remaining
printfn "Logs: %A" logs

let initialEnv: Env<int, int> = Env.intial 0

let result = stmt.Exec initialEnv