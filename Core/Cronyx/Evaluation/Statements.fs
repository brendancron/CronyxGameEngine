namespace Cronyx.Evaluation

open Cronyx.Core.Effects
open Cronyx.Evaluation.Grammar
open Cronyx.Evaluation.Environment

module Statements = 

    type EffectStmt<'state, 'effect, 'event>
        (effectEngine: IEffectEngine<'state, 'effect, 'event>,
        effectExpr: IExpr<'effect, 'state, 'effect, 'event>) =
        interface IStmt<'state, 'effect, 'event> with
            member _.Exec env =
                let effect = effectExpr.Eval env
                let preProcessedEffect = effectEngine.EffectPreProcessor env.GameState effect
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
                    |> List.fold (fun accEnv (stmt, provId) ->
                        if accEnv.Provenance.Contains provId then
                            accEnv
                        else
                            let provEnv = { accEnv with Provenance = accEnv.Provenance.Add provId }
                            stmt.Exec provEnv
                    ) env'

    (*
        Expression statement: evaluate, discard result
    *)
    type ExprStmt<'a, 'state, 'effect, 'event>
        (expr: IExpr<'a, 'state, 'effect, 'event>) =
        interface IStmt<'state, 'effect, 'event> with
            member _.Exec env =
                expr.Eval env |> ignore
                env
        override this.ToString() =
            sprintf "ExprStmt(%A)" expr

    (*
       Allows function processing of the expression result (e.g., printing to console)
    *)
    type FnStmt<'a, 'state, 'effect, 'event>
        (expr: IExpr<'a, 'state, 'effect, 'event>,
        fn: 'a -> unit) =
        interface IStmt<'state, 'effect, 'event> with
            member _.Exec env =
                fn (expr.Eval env)
                env
        override this.ToString() =
            sprintf "FnStmt(%A)" expr

   (*
       Sequential block of statements with its own lexical scope
   *)
   type BlockStmt<'state, 'effect, 'event>
       (stmts: IStmt<'state, 'effect, 'event> list) =
       interface IStmt<'state, 'effect, 'event> with
           member _.Exec env0 =
               let env1 = Env.push env0
               let env2 = (env1, stmts) ||> List.fold (fun e s -> s.Exec e)
               Env.pop env2

   (*
       Control flow
   *)
   type IfStmt<'state, 'effect, 'event>
       (cond: IExpr<bool, 'state, 'effect, 'event>,
        thenBranch: IStmt<'state, 'effect, 'event>,
        elseBranch: IStmt<'state, 'effect, 'event> option) =
       interface IStmt<'state, 'effect, 'event> with
           member _.Exec env =
               if cond.Eval env then
                   thenBranch.Exec env
               else
                   match elseBranch with
                   | Some e -> e.Exec env
                   | None -> env


   type WhileStmt<'state, 'effect, 'event>
       (cond: IExpr<bool, 'state, 'effect, 'event>,
        body: IStmt<'state, 'effect, 'event>) =
       interface IStmt<'state, 'effect, 'event> with
           member _.Exec env0 =
               let rec loop env =
                   if cond.Eval env then loop (body.Exec env) else env
               loop env0


   type ForStmt<'state, 'effect, 'event>
       (init: IStmt<'state, 'effect, 'event> option,
        cond: IExpr<bool, 'state, 'effect, 'event>,
        increment: IStmt<'state, 'effect, 'event> option,
        body: IStmt<'state, 'effect, 'event>) =
       interface IStmt<'state, 'effect, 'event> with
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
   type VarDeclStmt<'a, 'state, 'effect, 'event>
       (name: string, 
       init: IExpr<'a, 'state, 'effect, 'event>) =
       interface IStmt<'state, 'effect, 'event> with
           member _.Exec env =
               let value = init.Eval env |> box
               Env.define name value env


   type AssignStmt<'a, 'state, 'effect, 'event>
       (name: string, expr: IExpr<'a, 'state, 'effect, 'event>) =
       interface IStmt<'state, 'effect, 'event> with
           member _.Exec env =
               let value = expr.Eval env |> box
               Env.assign name value env