namespace Cronyx

open Cronyx.Effects

module Expressions = 
    type IExpr<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>> =
        abstract member Eval : Env<'eff,'event,'state> -> 'a

    (*
        Utility eval expression to allow for simpler calling of expression evaluations
    *)

    let eval (expr: IExpr<'a,'eff,'event,'state>) (env: Env<'eff,'event,'state>) : 'a =
        expr.Eval env

    (*
        Primary Type definitions go here
    *)

    type BoolExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>> (b: bool) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval _ = b

    type IntExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>> (i: int) =
        interface IExpr<int,'eff,'event,'state> with
            member _.Eval _ = i

    type StringExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>> (str: string) =
        interface IExpr<string,'eff,'event,'state> with
            member _.Eval _ = str

    type FloatExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>> (f: float) =
        interface IExpr<float,'eff,'event,'state> with
            member _.Eval _ = f

    (*
        Logic Operators
    *)

    type AndExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<bool,'eff,'event,'state>, r: IExpr<bool,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = if l.Eval env then r.Eval env else false

    type OrExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<bool,'eff,'event,'state>, r: IExpr<bool,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = if l.Eval env then true else r.Eval env

    type XorExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<bool,'eff,'event,'state>, r: IExpr<bool,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = l.Eval env <> r.Eval env

    type NotExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (e: IExpr<bool,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = not (e.Eval env)

    (*
        Arithmetic Operators

        NOTE: In F# we cannot constrain a generic type `'a` to “supports (+)” directly.
        Static member constraints (SRTP) only work on `inline` functions, not on type
        definitions. Because of that, we must explicitly pass in the operator function
        (`add: 'a -> 'a -> 'a`) to the constructor. This allows `AddExpr` to work with
        any type that defines a valid addition function, including built-in numerics
        and custom types (like vectors).

        Additionally, I am aware I could reduce this to UnaryExpr and BinaryExpr, however
        I think that the addition of seperate expressions will make it easier to debug
    *)

    type AddExpr<'l,'r,'res,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<'l,'eff,'event,'state>, r: IExpr<'r,'eff,'event,'state>, add: 'l -> 'r -> 'res) =
        interface IExpr<'res,'eff,'event,'state> with
            member _.Eval env = add (l.Eval env) (r.Eval env)

    type SubExpr<'l,'r,'res,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<'l,'eff,'event,'state>, r: IExpr<'r,'eff,'event,'state>, sub: 'l -> 'r -> 'res) =
        interface IExpr<'res,'eff,'event,'state> with
            member _.Eval env = sub (l.Eval env) (r.Eval env)

    type MulExpr<'l,'r,'res,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<'l,'eff,'event,'state>, r: IExpr<'r,'eff,'event,'state>, mul: 'l -> 'r -> 'res) =
        interface IExpr<'res,'eff,'event,'state> with
            member _.Eval env = mul (l.Eval env) (r.Eval env)

    type DivExpr<'l,'r,'res,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<'l,'eff,'event,'state>, r: IExpr<'r,'eff,'event,'state>, div: 'l -> 'r -> 'res) =
        interface IExpr<'res,'eff,'event,'state> with
            member _.Eval env = div (l.Eval env) (r.Eval env)

    type ModExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (l: IExpr<int,'eff,'event,'state>, r: IExpr<int,'eff,'event,'state>) =
        interface IExpr<int,'eff,'event,'state> with
            member _.Eval env = l.Eval env % r.Eval env

    type AbsExpr<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (e: IExpr<'a,'eff,'event,'state>, abs: 'a -> 'a) =
        interface IExpr<'a,'eff,'event,'state> with
            member _.Eval env = abs (e.Eval env)

    type NegExpr<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (e: IExpr<'a,'eff,'event,'state>, neg: 'a -> 'a) =
        interface IExpr<'a,'eff,'event,'state> with
            member _.Eval env = neg (e.Eval env)


    (*
        Comparison Expressions
    *)

    type EqualExpr<'a,'eff,'event,'state when 'a : equality and 'state :> IGameState<'eff,'event>>
        (l: IExpr<'a,'eff,'event,'state>, r: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = (l.Eval env = r.Eval env)

    type NotEqualExpr<'a,'eff,'event,'state when 'a : equality and 'state :> IGameState<'eff,'event>>
        (l: IExpr<'a,'eff,'event,'state>, r: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = not (l.Eval env = r.Eval env)

    type GreaterThanExpr<'a,'eff,'event,'state when 'a : comparison and 'state :> IGameState<'eff,'event>>
        (l: IExpr<'a,'eff,'event,'state>, r: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = (l.Eval env > r.Eval env)

    type LessThanExpr<'a,'eff,'event,'state when 'a : comparison and 'state :> IGameState<'eff,'event>>
        (l: IExpr<'a,'eff,'event,'state>, r: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = (l.Eval env < r.Eval env)

    type GreaterOrEqualExpr<'a,'eff,'event,'state when 'a : comparison and 'state :> IGameState<'eff,'event>>
        (l: IExpr<'a,'eff,'event,'state>, r: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = (l.Eval env >= r.Eval env)

    type LessOrEqualExpr<'a,'eff,'event,'state when 'a : comparison and 'state :> IGameState<'eff,'event>>
        (l: IExpr<'a,'eff,'event,'state>, r: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<bool,'eff,'event,'state> with
            member _.Eval env = (l.Eval env <= r.Eval env)

    (*
        Conditional Expressions
    *)

    type TernaryExpr<'r,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (c: IExpr<bool,'eff,'event,'state>, t: IExpr<'r,'eff,'event,'state>, e: IExpr<'r,'eff,'event,'state>) =
        interface IExpr<'r,'eff,'event,'state> with
            member _.Eval env = if c.Eval env then t.Eval env else e.Eval env


    (*
        Lambda
    *)

    type LambdaExpr<'a,'b,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (f: 'a -> 'b) =
        interface IExpr<'a -> 'b,'eff,'event,'state> with
            member _.Eval _ = f

    type ApplyExpr<'a,'b,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (f: IExpr<'a -> 'b,'eff,'event,'state>, arg: IExpr<'a,'eff,'event,'state>) =
        interface IExpr<'b,'eff,'event,'state> with
            member _.Eval env = (f.Eval env) (arg.Eval env)

    (*
        Variables
    *)

    type VarExpr<'a,'eff,'event,'state when 'state :> IGameState<'eff,'event>>
        (name: string) =
        interface IExpr<'a,'eff,'event,'state> with
            member _.Eval env = Env.get name env |> unbox<'a>

    (*
        Queries
    *)

    type FoldEventsExpr<'event,'a,'eff,'state when 'state :> IGameState<'eff,'event>>
        (folder   : 'a -> 'event -> 'a,
         seed     : 'a) =
        interface IExpr<'a,'eff,'event,'state> with
            member _.Eval env =
                env.Trace |> List.fold folder seed