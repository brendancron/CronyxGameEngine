open Cronyx.Core.Effects
open Cronyx.DSL.Grammar
open Cronyx.DSL.Expressions
open Cronyx.DSL.Statements
open Cronyx.DSL.Environment

(* Example Game definition *)

type PlayerId = string

type Effect =
  | Damage of player: PlayerId * amount: int
  | Heal   of player: PlayerId * amount: int

type Event = 
  | DamageResolved of player: PlayerId * amount: int
  | HealResolved of player: PlayerId * amount: int

type IEventTrigger<'state,'effect,'event> =
    abstract member Id : string
    abstract member TryFire : 'state -> 'event -> IStmt<'state,'effect,'event> option

type State = {
    PlayerMap: Map<PlayerId, int>
    Modifiers: (Effect -> Effect) list
    Triggers  : IEventTrigger<State, Effect, Event> list
}

type State with
    member this.PlayerExists(playerId: PlayerId) =
        this.PlayerMap.ContainsKey(playerId)

type EffectEngine() = 
    interface IEffectEngine<State, Effect, Event> with
        member this.EffectPreProcessor state effect = 
            List.fold (fun acc modify -> modify acc) effect state.Modifiers

        member this.EffectValidator state effect = 
            match effect with
            | Damage (pid, amt)
            | Heal   (pid, amt) ->
                state.PlayerExists pid && amt >= 0
        
        member this.EffectApplier state effect =
            match effect with
            | Damage (pid, amt) ->
                let cur = state.PlayerMap[pid]
                let pmap = state.PlayerMap |> Map.add pid (cur - amt)
                { state with PlayerMap = pmap }, [DamageResolved(pid, amt)]
            | Heal (pid, amt) ->
                let cur = state.PlayerMap[pid]
                let pmap = state.PlayerMap |> Map.add pid (cur + amt)
                { state with PlayerMap = pmap }, [HealResolved(pid, amt)]
        
        member this.EffectPostProcessor (state: State) (events: Event list)
            : (IStmt<State, Effect, Event> * string) list =
            events
            |> List.collect (fun ev ->
                state.Triggers
                |> List.choose (fun trig ->
                    trig.TryFire state ev
                    |> Option.map (fun stmt -> (stmt, trig.Id))))

let effectEngine = EffectEngine()

(* Modifier Definitions *)

let healingScale (percent: float) (target: PlayerId) (effect: Effect) = 
    match effect with
    | Heal (player, amt) when player = target -> Heal (player, int(float(amt) * percent))
    | other    -> other

(* Trigger Definitions *)

let reflectDamage
    (id: string)
    (fromPid: PlayerId)
    (toPid: PlayerId)
    (percent: float) =
    { new IEventTrigger<State, Effect, Event> with
        member _.Id = id
        member _.TryFire state event =
            match event with
            | DamageResolved (pid, amt) when pid = fromPid && amt > 0 && percent > 0.0 ->
                let reflectedAmt = int(float(amt) * percent)
                if reflectedAmt <= 0 then None else
                    let stmt = EffectStmt(effectEngine, EffectExpr(Damage(toPid, reflectedAmt)))
                    Some stmt
            | _ -> None }

(* Test Section *)

let initialState = {
    PlayerMap = [
        "Alice", 100
        "Bob", 100 ]
        |> Map.ofList
    Modifiers = [(healingScale 2.0 "Bob")]
    Triggers = [
        (reflectDamage "reflectAB" "Alice" "Bob" 0.5)
        (reflectDamage "reflectBA" "Bob" "Alice" 0.5)
    ]
}

let damageEffect = Damage("Alice", 30)
let damageExpr = EffectExpr(damageEffect)
let damageStmt: IStmt<_,_,_> = EffectStmt(effectEngine, damageExpr)

let env : Env<State, Event> = Env.intial initialState

let healEffect = Heal("Bob", 25)
let healExpr = EffectExpr(healEffect)
let healStmt: IStmt<_,_,_> = EffectStmt(effectEngine, healExpr)

let intEnv = damageStmt.Exec env
let finalEnv = healStmt.Exec intEnv

printfn "Initial state: %A" initialState.PlayerMap
printfn "Final state: %A" finalEnv.GameState.PlayerMap
printfn "Events: %A" finalEnv.Trace

//let reflectAliceOnBob = ReflectDamage("reflect-Alice-Bob", "Alice", "Bob", 0.5)
//let reflectBobOnAlice = ReflectDamage("reflect-Bob-Alice", "Bob", "Alice", 0.5)

//let doubleHeal = GlobalHealingScale("Global Heal", 2.0)

//// State Construction

    //Modifiers = [doubleHeal]
    //Triggers = [reflectAliceOnBob;reflectBobOnAlice]
//}

//// Giga drain example

//let gigaDrain self target healRatio : Stmt =
//    BlockStmt([
//        EffectStmt(DamageExpr(target, IntExpr 50), validate_effect, apply_effect) :> Stmt
//        EffectStmt(HealExpr(self,
//            MulExpr(
//                FoldEventsExpr(
//                    (fun acc ev ->
//                        match ev with
//                        | DamageResolved(pid, amt) when pid = target -> acc + amt
//                        | _ -> acc),
//                    0),
//                FloatExpr healRatio,
//                (fun damage ratio -> int (float damage * ratio))
//            )
//        ), validate_effect, apply_effect) :> Stmt
//    ]) :> Stmt

//// Usage

//let env = Env.empty initialState
//let gigaDrainStmt = gigaDrain "Alice" "Bob" 0.5
//let finalEnv = gigaDrainStmt.Exec env

//printfn "Initial state: %A" initialState.PlayerMap
//printfn "Final state: %A" finalEnv.GameState.PlayerMap
//printfn "Events: %A" finalEnv.Trace