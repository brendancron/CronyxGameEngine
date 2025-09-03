namespace Cronyx.Core

type TaggedEffect<'eff> = {
    Effect : 'eff
    Provenance : Set<string> }

(*
    Effect Modifiers simply will modify the current effect, leaving it unchanged
*)

type IEffectModifier<'eff> =
    abstract member Id : string
    abstract member Modify: 'eff -> 'eff

(*
    Event Triggers will spawn new logic to be executed once current effects are modified
*)

and IEventTrigger<'eff,'event> =
    abstract member Id : string
    abstract member OnEvent : 'event -> IStmt<'eff,'event,'state> option

and IGameState<'eff, 'event> = 
    abstract member Modifiers: IEffectModifier<'eff> list
    abstract member Triggers: IEventTrigger<'eff, 'event> list

// DSL

and Env<'eff,'event,'state when 'state :> IGameState<'eff,'event>> = {
    Scopes     : Map<string,obj> list
    GameState  : 'state
    Trace      : 'event list
    Provenance : Set<string> list
}

and IExpr<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>> =
    abstract member Eval : Env<'eff,'event,'state> -> 'a

and IStmt<'eff,'event,'state when 'state :> IGameState<'eff,'event>> =
    abstract member Exec : Env<'eff,'event,'state> -> Env<'eff,'event,'state>