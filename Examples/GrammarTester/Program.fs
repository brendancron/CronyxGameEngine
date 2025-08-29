open Cronyx.Expressions
open Cronyx.Statements
open Cronyx

let block =
    BlockStmt([
        VarDeclStmt("x", IntExpr 10) :> IStmt
        PrintStmt(VarExpr<int>("x")) :> IStmt
        BlockStmt([
            VarDeclStmt("x", IntExpr 20) :> IStmt
            PrintStmt(VarExpr<int>("x")) :> IStmt
        ]) :> IStmt
        PrintStmt(VarExpr<int>("x")) :> IStmt
        VarDeclStmt("y", IntExpr 0) :> IStmt
        WhileStmt(
            LessThanExpr(VarExpr<int>("y"), IntExpr 3),
            BlockStmt([
                PrintStmt(VarExpr<int>("y")) :> IStmt
                AssignStmt("y",
                    AddExpr(VarExpr<int>("y"), IntExpr 1, (+))
                ) :> IStmt
            ])
        ) :> IStmt
    ]) :> IStmt

let env0 = Env.empty
let env1 = block.Exec env0