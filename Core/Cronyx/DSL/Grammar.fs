namespace Cronyx.DSL

module Grammar =

    (* 
        Environment Definition

        Scopes - Allows for lexical scoping throughout interpretation

        GameState - Holds the current gamestate

        Trace - Provides a list of all events that occurred during exectuion, useful for animation/logging

        Provenance - Allows for a specialized trace of effect modification to prevent infinite loops
    *)

    type Env<'state, 'event> = {
        Scopes: Map<string, obj> list
        GameState: 'state
        Trace: 'event list
        Provenance: Set<string> list
    }

    module Env =
        let intial (initialState : 'state) : Env<'state, 'event> =
            { Scopes = [ Map.empty ]
              GameState = initialState
              Trace = []
              Provenance = [ Set.empty ] }
    (* 
        IExpr Definition

        Resolves to some type 'a
        Requires the current environment to execute but does not edit the environment itself
    *)

    type IExpr<'a, 'state, 'effect, 'event> = 
        abstract member Eval : Env<'state, 'event> -> 'a

    (* 
        IStmt Definition

        Modifys the environment in some way or determines control flow to other statements
    *)

    type IStmt<'state, 'effect, 'event> =
        abstract member Exec : Env<'state, 'event> -> Env<'state, 'event>

    (* 
        IEffectEngine Definition

        This is the portion of the game an implementor is responsible for implementing

        EffectPreProcessor - Contains any modification logic required for the initial effect to be calculated

        EffectValidator - Validates whether or not an effect should actually apply

        EffectApplier - Determines how the effect gets applied

        EffectPostProcessor - Determines the consequences of the applied effect, potentially cascading effects
    *)

    type IEffectEngine<'state, 'effect, 'event> =
        abstract member EffectPreProcessor: 'effect -> 'effect;
        abstract member EffectValidator: 'state -> 'effect -> bool;
        abstract member EffectApplier: 'state -> 'effect -> 'state * 'event list;
        abstract member EffectPostProcessor: 'state -> 'event list -> (IStmt<'state, 'effect, 'event> * string) list

    (* Expressions Section *)

    type EffectExpr<'state, 'effect, 'event>
        (effect: 'effect) =
        interface IExpr<'effect, 'state, 'effect, 'event> with
            member _.Eval env = 
                effect

    (* Statments Section *)

    type EffectStmt<'state, 'effect, 'event>
        (effectEngine: IEffectEngine<'state, 'effect, 'event>,
        effectExpr: IExpr<'effect, 'state, 'effect, 'event>) =
        interface IStmt<'state, 'effect, 'event> with
            member _.Exec env =
                let effect = effectExpr.Eval env
                let preProcessedEffect = effectEngine.EffectPreProcessor effect
                if not (effectEngine.EffectValidator env.GameState preProcessedEffect) then 
                    env
                else
                    let (state', events) = effectEngine.EffectApplier env.GameState preProcessedEffect
                    
                    let env' = 
                        { env with
                            GameState = state'
                            Trace = env.Trace @ events }

                    (* This part is a litte ambiguous since we want to use the unprocessed state to postProcess *)
                    let postProcessedStmts = effectEngine.EffectPostProcessor env.GameState events
                    postProcessedStmts
                    |> List.fold (fun accEnv (stmt, triggerId) ->
                        (* TODO resolve provenance issues later *)
                        stmt.Exec accEnv
                    ) env'