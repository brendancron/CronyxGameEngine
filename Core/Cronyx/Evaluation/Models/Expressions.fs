namespace Cronyx.Evaluation.Models

module Expressions =

    type Expr = 
        | VariableExpression of string
        | ApplicationExpression of Expr * Expr
        | AbstractionExpression of string * Expr
        | LetExpression of string * Expr * Expr
        | ConstExpression of Const

    and Const =
        | IntConst of int
        | BoolConst of bool
        | StringConst of string
        | FloatConst of float