namespace Cronyx.Util

open Cronyx.Core

module Dummy =

    type DummyEffect = unit
    type DummyEvent  = unit

    type DummyState = {
        Modifiers : IEffectModifier<DummyEffect> list
        Triggers  : IEventTrigger<DummyEffect,DummyEvent,DummyState> list
    }
    with
        interface IGameState<DummyEffect,DummyEvent,DummyState> with
            member this.Modifiers = this.Modifiers
            member this.Triggers  = this.Triggers

    let emptyState : DummyState =
        { Modifiers = []; Triggers = [] }

    let emptyEnv () = Env.empty emptyState
