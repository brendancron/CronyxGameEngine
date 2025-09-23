namespace Cronyx.Core

open Cronyx.Evaluation.Grammar

module Effects = 

    (* 
        IEffectEngine Definition

        This is the portion of the game an implementor is responsible for implementing

        EffectPreProcessor - Contains any modification logic required for the initial effect to be calculated

        EffectValidator - Validates whether or not an effect should actually apply

        EffectApplier - Determines how the effect gets applied

        EffectPostProcessor - Determines the consequences of the applied effect, potentially cascading effects
    *)

    type IEffectEngine<'state, 'effect, 'event> =
        abstract member EffectPreProcessor: 'state -> 'effect -> 'effect;
        abstract member EffectValidator: 'state -> 'effect -> bool;
        abstract member EffectApplier: 'state -> 'effect -> 'state * 'event list;
        abstract member EffectPostProcessor: 'state -> 'event list -> (IStmt<'state, 'effect, 'event> * string) list