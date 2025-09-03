namespace Cronyx

open Cronyx.Core
open Cronyx.Effects

module Statements = 
    (*
        Expression statement: evaluate, discard result
    *)
    type ExprStmt<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (expr: IExpr<'a,'eff,'event,'state>) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env =
                expr.Eval env |> ignore
                env


    (*
        Sequential block of statements with its own lexical scope
    *)
    type BlockStmt<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (stmts: IStmt<'eff,'event,'state> list) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env0 =
                let env1 = Env.push env0
                let env2 = (env1, stmts) ||> List.fold (fun e s -> s.Exec e)
                Env.pop env2

    (*
        Debug printing
    *)
    type PrintStmt<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (expr: IExpr<'a,'eff,'event,'state>) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env =
                printfn "%A" (expr.Eval env)
                env


    (*
        Control flow
    *)
    type IfStmt<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (cond: IExpr<bool,'eff,'event,'state>,
         thenBranch: IStmt<'eff,'event,'state>,
         elseBranch: IStmt<'eff,'event,'state> option) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env =
                if cond.Eval env then
                    thenBranch.Exec env
                else
                    match elseBranch with
                    | Some e -> e.Exec env
                    | None -> env


    type WhileStmt<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (cond: IExpr<bool,'eff,'event,'state>,
         body: IStmt<'eff,'event,'state>) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env0 =
                let rec loop env =
                    if cond.Eval env then loop (body.Exec env) else env
                loop env0


    type ForStmt<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (init: IStmt<'eff,'event,'state> option,
         cond: IExpr<bool,'eff,'event,'state>,
         increment: IStmt<'eff,'event,'state> option,
         body: IStmt<'eff,'event,'state>) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env0 =
                let env1 =
                    match init with
                    | Some i -> i.Exec env0
                    | None   -> env0
                let rec loop env =
                    if cond.Eval env then
                        let afterBody = body.Exec env
                        let afterInc =
                            match increment with
                            | Some inc -> inc.Exec afterBody
                            | None     -> afterBody
                        loop afterInc
                    else env
                loop env1


    (*
        Variables
    *)
    type VarDeclStmt<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (name: string, init: IExpr<'a,'eff,'event,'state>) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env =
                let value = init.Eval env |> box
                Env.define name value env


    type AssignStmt<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (name: string, expr: IExpr<'a,'eff,'event,'state>) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env =
                let value = expr.Eval env |> box
                Env.assign name value env


    (*
        Game Effects
    *)

    type EffectStmt<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (effectExpr: IExpr<'eff,'eff,'event,'state>,
         validate: 'eff -> 'state -> bool,
         apply: 'eff -> 'state -> 'state * 'event list,
         ?onInvalid: string -> unit) =
        interface IStmt<'eff,'event,'state> with
            member _.Exec env =
                // Step 1: Resolve the effect
                let effect = effectExpr.Eval env
                // we want to use the original triggers to spawn new events
                let originalTriggers = env.GameState.Triggers
                // Step 2: Resolve all modifiers
                let modified_effect, provenance =
                    applyModifiers env.GameState.Modifiers effect env.Provenance
                // Step 3: Determine validity
                if (validate modified_effect env.GameState) then
                    // Step 4: Apply effect
                    let state', events = apply modified_effect env.GameState

                    // Step 5: Collect trigger-spawned statements
                    let triggeredStmts = applyTriggers originalTriggers events

                    // Step 6: Update environment with new state + events
                    let env' = 
                        { env with
                            GameState = state'
                            Trace = env.Trace @ events
                            Provenance = provenance }

                    // Step 7: Execute each triggered statement,
                    // pushing trigger ID into provenance
                    triggeredStmts
                    |> List.fold (fun acc (stmt, triggerId) ->
                        let acc1 = Env.pushProvenance acc
                        let acc2 = Env.addProvenance triggerId acc1
                        let acc3 = stmt.Exec acc2
                        Env.popProvenance acc3
                    ) env'
                else
                    env