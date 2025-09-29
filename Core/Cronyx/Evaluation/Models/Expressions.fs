namespace Cronyx.Evaluation.Models

module Expressions =

    type BinOp =
        | Add
        | Sub
        | Mul
        | Div

    type Expr = 
        | VariableExpression of string
        | ApplicationExpression of Expr * Expr
        | AbstractionExpression of string * Expr
        | LetExpression of string * Expr * Expr
        | ConstExpression of Const
        | BinaryExpression of BinOp * Expr * Expr

    and Const =
        | IntConst of int
        | BoolConst of bool
        | StringConst of string
        | FloatConst of float