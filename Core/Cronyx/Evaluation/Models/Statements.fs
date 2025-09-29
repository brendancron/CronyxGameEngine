
open Cronyx.Evaluation.Models.Expressions

module Statements =

    type Stmt =
    | Bind   of string * Expr
    | Expr   of Expr
    | If     of cond:Expr * thenS:Stmt * elseS:Stmt option
    | While  of cond:Expr * body:Stmt
    | For    of init:Stmt option * cond:Expr option * step:Expr option * body:Stmt
    | Block  of Stmt list

    type Program = Stmt list
