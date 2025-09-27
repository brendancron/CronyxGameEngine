namespace Cronyx.Evaluation.Components

open Cronyx.Evaluation.Models.Types
open Cronyx.Evaluation.Models.FreeTypes
open Cronyx.Evaluation.Models.Expressions
open Cronyx.Evaluation.Models.Substitutions
open Cronyx.Evaluation.Models.Substitutions.State

module Inference =

    exception TypeError of string

    /// Hindley–Milner W algorithm
    let rec W (ctx: Context) (expr: Expr) : State<int, Substitution * MonoType> =
        state {
            match expr with
            // --- variable lookup ---
            | VariableExpression x ->
                match Map.tryFind x ctx with
                | Some sigma ->
                    let! tau = instantiate sigma
                    return (Map.empty, tau)
                | None ->
                    return raise (TypeError (sprintf "Unbound variable: %s" x))

            // --- constant ---
            | ConstExpression c ->
                match c with
                | IntConst _    -> return (Map.empty, TypeFunctionApplication (PrimitiveType Int))
                | BoolConst _   -> return (Map.empty, TypeFunctionApplication (PrimitiveType Bool))
                | StringConst _ -> return (Map.empty, TypeFunctionApplication (PrimitiveType String))
                | FloatConst _  -> return (Map.empty, TypeFunctionApplication (PrimitiveType Float))

            // --- abstraction λx.e ---
            | AbstractionExpression (x, e) ->
                let! alpha = freshTypeVar
                let ctx' = Map.add x (Mono alpha) ctx
                let! (s1, t1) = W ctx' e
                let argType = applySubstitutionToMonoType s1 alpha
                let funType = TypeFunctionApplication (Arrow (argType, t1))
                return (s1, funType)

            // --- application e1 e2 ---
            | ApplicationExpression (e1, e2) ->
                let! (s1, t1) = W ctx e1
                let ctx' = applySubstitutionToContext s1 ctx
                let! (s2, t2) = W ctx' e2
                let! alpha = freshTypeVar
                let s3 = unify (applySubstitutionToMonoType s2 t1)
                               (TypeFunctionApplication (Arrow (t2, alpha)))
                let s = combine (combine s3 s2) s1
                let t = applySubstitutionToMonoType s3 alpha
                return (s, t)

            // --- let x = e1 in e2 ---
            | LetExpression (x, e1, e2) ->
                let! (s1, t1) = W ctx e1
                let ctx' = applySubstitutionToContext s1 ctx
                let sigma = generalize ctx' t1
                let ctx'' = Map.add x sigma ctx'
                let! (s2, t2) = W ctx'' e2
                let s = combine s2 s1
                return (s, t2)
        }

    /// Convenience wrapper
    let inferType (ctx: Context) (expr: Expr) : Substitution * MonoType =
        State.eval (W ctx expr) 0