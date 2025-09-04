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
        Triggers: IEventTrigger<Effect, Event, State> list
    }
    interface IGameState<Effect, Event, State> with
        member this.Modifiers = this.Modifiers
        member this.Triggers = this.Triggers

type Expr<'a> = IExpr<'a,Effect,Event,State>
type Stmt = IStmt<Effect,Event,State>
type Env = Env<Effect,Event,State>

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

type HealExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
    (player: PlayerId, amount: IExpr<int,'eff,'event,'state>) =
    interface IExpr<Effect,'eff,'event,'state> with
        member _.Eval env =
            let amt = amount.Eval env
            Heal(player, amt)

type DamageExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
    (player: PlayerId, amount: IExpr<int,'eff,'event,'state>) =
    interface IExpr<Effect,'eff,'event,'state> with
        member _.Eval env =
            let amt = amount.Eval env
            Damage(player, amt)

type ReflectDamage(id: string, target: PlayerId, source: PlayerId, percent: float) =
    interface IEventTrigger<Effect, Event, State> with
        member _.Id = id
        member _.OnEvent ev =
            match ev with
            | DamageResolved (pid, amt) when pid = source ->
                let reflectedAmt = int (float amt * percent)
                Some (
                    EffectStmt(
                        DamageExpr(target, IntExpr reflectedAmt),
                        validate_effect,
                        apply_effect
                    ) :> Stmt
                )
            | _ -> None

let reflectAliceOnBob = ReflectDamage("reflect-Alice-Bob", "Alice", "Bob", 0.5)
let reflectBobOnAlice = ReflectDamage("reflect-Bob-Alice", "Bob", "Alice", 0.5)

let doubleHeal = GlobalHealingScale("Global Heal", 2.0)

// State Construction

let initialState = {
    PlayerMap = [
        "Alice", 100
        "Bob", 100 ]
        |> Map.ofList
    Modifiers = [doubleHeal]
    Triggers = [reflectAliceOnBob]
}

// Giga drain example

let gigaDrain self target healRatio : Stmt =
    BlockStmt([
        EffectStmt(DamageExpr(target, IntExpr 50), validate_effect, apply_effect) :> Stmt
        EffectStmt(HealExpr(self,
            MulExpr(
                FoldEventsExpr(
                    (fun acc ev ->
                        match ev with
                        | DamageResolved(pid, amt) when pid = target -> acc + amt
                        | _ -> acc),
                    0),
                FloatExpr healRatio,
                (fun damage ratio -> int (float damage * ratio))
            )
        ), validate_effect, apply_effect) :> Stmt
    ]) :> Stmt

// Usage

let env = Env.empty initialState
let gigaDrainStmt = gigaDrain "Alice" "Bob" 0.5
let finalEnv = gigaDrainStmt.Exec env

printfn "Initial state: %A" initialState.PlayerMap
printfn "Final state: %A" finalEnv.GameState.PlayerMap
printfn "Events: %A" finalEnv.Trace