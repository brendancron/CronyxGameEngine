open Cronyx.Expressions
open Cronyx.Statements
open Cronyx
open Cronyx.Core

// --- Minimal dummy state so Env<'eff,'event,'state> can compile ---
type DummyEffect = unit
type DummyEvent  = string

type DummyState() =
    interface IGameState<DummyEffect,DummyEvent> with
        member _.Modifiers = []
        member _.Triggers  = []

let initialState = DummyState()

// --- Build a program block using the generic forms ---
let block =
    BlockStmt<DummyEffect,DummyEvent,DummyState>([
        VarDeclStmt<int,DummyEffect,DummyEvent,DummyState>("x", IntExpr<DummyEffect,DummyEvent,DummyState>(10)) :> IStmt<_,_,_>
        PrintStmt<int,DummyEffect,DummyEvent,DummyState>(VarExpr<int,DummyEffect,DummyEvent,DummyState>("x")) :> IStmt<_,_,_>
        BlockStmt<DummyEffect,DummyEvent,DummyState>([
            VarDeclStmt<int,_,_,_>("x", IntExpr<_,_,_>(20)) :> IStmt<_,_,_>
            PrintStmt<int,_,_,_>(VarExpr<int,_,_,_>("x")) :> IStmt<_,_,_>
        ]) :> IStmt<_,_,_>
        PrintStmt<int,_,_,_>(VarExpr<int,_,_,_>("x")) :> IStmt<_,_,_>
        VarDeclStmt<int,_,_,_>("y", IntExpr<_,_,_>(0)) :> IStmt<_,_,_>
        WhileStmt<DummyEffect,DummyEvent,DummyState>(
            LessThanExpr<int,_,_,_>(VarExpr<int,_,_,_>("y"), IntExpr<_,_,_>(3)),
            BlockStmt<DummyEffect,DummyEvent,DummyState>([
                PrintStmt<int,_,_,_>(VarExpr<int,_,_,_>("y")) :> IStmt<_,_,_>
                AssignStmt<int,_,_,_>("y",
                    AddExpr<int,int,int,_,_,_>(VarExpr<int,_,_,_>("y"), IntExpr<_,_,_>(1), (+))
                ) :> IStmt<_,_,_>
            ])
        ) :> IStmt<_,_,_>
    ]) :> IStmt<_,_,_>

// --- Run it ---
let env0 = Env.empty initialState
let env1 = block.Exec env0