open Cronyx.Core
open Cronyx.Parsing
open Cronyx.Parsing.Parser

type Effect = unit
type Event  = unit

type State = {
    Modifiers : IEffectModifier<Effect> list
    Triggers  : IEventTrigger<Effect,Event,State> list
} with
    interface IGameState<Effect,Event,State> with
        member this.Modifiers = this.Modifiers
        member this.Triggers  = this.Triggers

let initialState : State =
  { Modifiers = []; Triggers = [] }

let source = "3 + 4 * 5"

let tokens = Lexer.scan source

printf "%A" (tokens)

let expr, rest = parseArithmetic<Effect,Event,State> tokens

let env = Env.empty initialState
let value : int = expr.Eval env

printfn "Tokens: %A" (tokens |> List.map (fun t -> t.TokenType, t.Lexeme))
printfn "Value: %d" value