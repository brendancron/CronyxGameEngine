//open Cronyx.Core
//open Cronyx.Parsing
//open Cronyx.Parsing.Parser
//open Cronyx.Util.Dummy

//let source = "3 + 4 * 5"

//let tokens = Lexer.scan source

//printf "%A" (tokens)

//let expr, rest = parseArithmetic<DummyEffect,DummyEvent,DummyState> tokens

//let env = Env.empty emptyState
//let value : int = expr.Eval env

//printfn "Tokens: %A" (tokens |> List.map (fun t -> t.TokenType, t.Lexeme))
//printfn "Value: %d" value

open Cronyx.DSL



printf "%A" (7)