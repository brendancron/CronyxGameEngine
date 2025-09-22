open Cronyx.DSL

open Cronyx.Parsing.Lexer
open Cronyx.Parsing.Parser
open Cronyx.DSL.Grammar
open Cronyx.DSL.Environment

let source = "3 + 4 * 5"

let tokens = scan source

printfn "Tokens: %A" (tokens |> List.map (fun t -> t.TokenType, t.Lexeme))

let (expr: IExpr<int, string, string, string>), remaining, logs = parse<string,string,string> tokens

printfn "Expr: %A" expr
printfn "Remaining Tokens: %A" remaining
printfn "Logs: %A" logs

let initialEnv: Env<string, string> = Env.intial "hello"

let result = expr.Eval initialEnv
printfn "Result: %A" result