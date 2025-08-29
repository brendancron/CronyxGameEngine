namespace Cronyx

open Cronyx.Expressions

module Statements = 
    type IStmt =
        abstract member Exec : Env -> Env

    let exec (stmt : IStmt) (env: Env) = stmt.Exec env

    (*
        Adding the ability for expression statements
    *)

    type ExprStmt<'a> (expr: IExpr<'a>) = 
        interface IStmt with
            member _.Exec env =
                expr.Eval env |> ignore
                env

    (*
        allows for a list of statements to be run sequentially
    *)

    type BlockStmt(stmts: IStmt list) =
        interface IStmt with
            member _.Exec env0 =
                let env1 = Env.push env0
                let env2 = (env1, stmts) ||> List.fold (fun e s -> s.Exec e)
                Env.pop env2

    (*
        Added debugging logic
    *)

    type PrintStmt<'a>(expr: IExpr<'a>) =
        interface IStmt with
            member _.Exec env =
                printfn "%A" (expr.Eval env)
                env

    (*
        Control Flow
    *)

    type IfStmt(cond: IExpr<bool>, thenBranch: IStmt, elseBranch: IStmt option) =
        interface IStmt with
            member _.Exec env =
                if cond.Eval env then
                    thenBranch.Exec env
                else
                    match elseBranch with
                    | Some e -> e.Exec env
                    | None -> env


    type WhileStmt(cond: IExpr<bool>, body: IStmt) =
        interface IStmt with
            member _.Exec env0 =
                let rec loop env =
                    if cond.Eval env then loop (body.Exec env) else env
                loop env0

    type ForStmt(init: IStmt option, cond: IExpr<bool>, increment: IStmt option, body: IStmt) =
        interface IStmt with
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

    type VarDeclStmt<'a>(name: string, init: IExpr<'a>) =
        // Defines (or shadows) in the *current* scope
        interface IStmt with
            member _.Exec env =
                let value = init.Eval env |> box
                Env.define name value env

    type AssignStmt<'a>(name: string, expr: IExpr<'a>) =
        // Reassigns in the nearest scope where 'name' exists
        interface IStmt with
            member _.Exec env =
                let value = expr.Eval env |> box
                Env.assign name value env