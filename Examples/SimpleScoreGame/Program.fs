open Cronyx.Effects
open Cronyx.Expressions
open Cronyx.Statements
open Cronyx.Core

type PlayerId = string

type Effect =
  | Damage of player: PlayerId * amount: int
  | Heal   of player: PlayerId * amount: int

type Event = 
  | DamageResolved of player: PlayerId * amount: int
  | HealResolved of player: PlayerId * amount: int

type State = 
    {
        PlayerMap: Map<PlayerId, int>
        Modifiers: IEffectModifier<Effect> list
        Triggers: IEventTrigger<Effect, Event> list
    }
    interface IGameState<Effect, Event> with
        member this.Modifiers = this.Modifiers
        member this.Triggers = this.Triggers

let validate_effect (effect: Effect) (state: State) : bool =
  let playerExists pid = Map.containsKey pid state.PlayerMap
  match effect with
  | Damage (pid, amt)
  | Heal   (pid, amt) ->
      playerExists pid && amt >= 0

let apply_effect (effect: Effect) (state: State) : State * Event list =
  match effect with
  | Damage (pid, amt) ->
      let cur = state.PlayerMap[pid]
      let pmap = state.PlayerMap |> Map.add pid (cur - amt)
      { state with PlayerMap = pmap }, [DamageResolved(pid, amt)]
  | Heal (pid, amt) ->
      let cur = state.PlayerMap[pid]
      let pmap = state.PlayerMap |> Map.add pid (cur + amt)
      { state with PlayerMap = pmap }, [HealResolved(pid, amt)]

// Modifier Definitions

type HealingScale(id: string, target: string, percent: float) =
    interface IEffectModifier<Effect> with
        member _.Id = id
        member _.Modify(effect: Effect) =
            match effect with
            | Heal (player, amt) when player = target -> Heal (player, int(float(amt) * percent))
            | other    -> other

type GlobalHealingScale(id: string, percent: float) =
    interface IEffectModifier<Effect> with
        member _.Id = id
        member _.Modify(effect: Effect) =
            match effect with
            | Heal (player, amt) -> Heal (player, int(float(amt) * percent))
            | other    -> other

(*
type ReflectDamage(id: string, target: string, source: string, percent: float) =
    interface IEventTrigger<Effect, Event> with
        member _.Id = id
        member _.OnEvent event = 
            match event with
            | DamageResolved (pid, amt) when pid = source -> [Damage(target, int(float(amt) * percent))]
            | _ -> []
*)

// Modifier Construction

//let siphonHeal = SiphonHeal("siphon", "Alice", "Bob", 0.25)
//let reflectDamage = ReflectDamage("reflect", "Alice", "Bob", 0.5)
//let reflectDamage2 = ReflectDamage("reflect2", "Bob", "Alice", 0.5)
let doubleHeal = GlobalHealingScale("Global Heal", 2.0)

// State Construction

let initialState = {
    PlayerMap = [
        "Alice", 100
        "Bob", 100 ]
        |> Map.ofList
    Modifiers = [doubleHeal]
    Triggers = []
}

// Expr Construction

let healExpr target amt = EffectExpr(Heal(target, amt), Set.empty)
let damageExpr target amt = EffectExpr(Damage(target, amt), Set.empty)

type HealExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
    (player: PlayerId, amount: IExpr<int,'eff,'event,'state>) =
    interface IExpr<Effect,'eff,'event,'state> with
        member _.Eval env =
            let amt = amount.Eval env
            Heal(player, amt)

type DamageExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
    (player: PlayerId, amount: IExpr<int,'eff,'event,'state>) =
    interface IExpr<Effect,'eff,'event,'state> with
        member _.Eval env =
            let amt = amount.Eval env
            Damage(player, amt)

// Giga drain example

let gigaDrain self target healRatio =
    BlockStmt<Effect,Event,State>([
        EffectStmt(DamageExpr(target, IntExpr<_,_,_>(50)), validate_effect, apply_effect) :> IStmt<_,_,_>
        EffectStmt(HealExpr(self, 
            MulExpr<int,float,int,Effect,Event,State>(
                FoldEventsExpr<Event,int,Effect,State>(
                    (fun acc ev ->
                        match ev with
                        | DamageResolved(pid, amt) when pid = target -> acc + amt
                        | _ -> acc),
                    0),
                FloatExpr<_,_,_>(healRatio),
                (fun damage ratio -> int(float(damage) * ratio))
            )), validate_effect, apply_effect) :> IStmt<_,_,_>
    ]) :> IStmt<_,_,_>

// Usage

let env = Env.empty initialState
let gigaDrainStmt = gigaDrain "Alice" "Bob" 0.5
let finalEnv = gigaDrainStmt.Exec env

printfn "Initial state: %A" initialState.PlayerMap
printfn "Final state: %A" finalEnv.GameState.PlayerMap
printfn "Events: %A" finalEnv.Trace