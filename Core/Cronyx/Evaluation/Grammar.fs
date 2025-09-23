namespace Cronyx.Evaluation

open Cronyx.Evaluation.Environment

module Grammar =
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