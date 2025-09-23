namespace Cronyx.Evaluation

open Cronyx.Evaluation.Environment
open Cronyx.Evaluation.Grammar

module Expressions =
    (*
        Primary Type definitions go here
    *)

    type BoolExpr<'state, 'effect, 'event> (b: bool) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval _ = b
        override this.ToString() =
            sprintf "BoolExpr(%b)" b

    type IntExpr<'state, 'effect, 'event> (i: int) =
        interface IExpr<int, 'state, 'effect, 'event> with
            member _.Eval _ = i
        override this.ToString() =
            sprintf "IntExpr(%d)" i

    type StringExpr<'state, 'effect, 'event> (str: string) =
        interface IExpr<string, 'state, 'effect, 'event> with
            member _.Eval _ = str
        override this.ToString() =
            sprintf "StringExpr(%s)" str

    type FloatExpr<'state, 'effect, 'event> (f: float) =
        interface IExpr<float, 'state, 'effect, 'event> with
            member _.Eval _ = f
        override this.ToString() =
            sprintf "FloatExpr(%f)" f

    type EffectExpr<'state, 'effect, 'event>
        (effect: 'effect) =
        interface IExpr<'effect, 'state, 'effect, 'event> with
            member _.Eval env = 
                effect
        override this.ToString() =
            sprintf "EffectExpr(%s)" (effect.ToString())

    (*
        Logic Operators
    *)

    type AndExpr<'state, 'effect, 'event>
        (l: IExpr<bool, 'state, 'effect, 'event>, r: IExpr<bool, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = if l.Eval env then r.Eval env else false

    type OrExpr<'state, 'effect, 'event>
        (l: IExpr<bool, 'state, 'effect, 'event>, r: IExpr<bool, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = if l.Eval env then true else r.Eval env

    type XorExpr<'state, 'effect, 'event>
        (l: IExpr<bool, 'state, 'effect, 'event>, r: IExpr<bool, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = l.Eval env <> r.Eval env

    type NotExpr<'state, 'effect, 'event>
        (e: IExpr<bool, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
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

    type AddExpr<'l, 'r, 'res, 'state, 'effect, 'event>
        (l: IExpr<'l, 'state, 'effect, 'event>, r: IExpr<'r, 'state, 'effect, 'event>, add: 'l -> 'r -> 'res) =
        interface IExpr<'res, 'state, 'effect, 'event> with
            member _.Eval env = add (l.Eval env) (r.Eval env)
        override this.ToString() =
            sprintf "AddExpr(%s, %s)" (l.ToString()) (r.ToString())

    type SubExpr<'l, 'r, 'res, 'state, 'effect, 'event>
        (l: IExpr<'l, 'state, 'effect, 'event>, r: IExpr<'r, 'state, 'effect, 'event>, sub: 'l -> 'r -> 'res) =
        interface IExpr<'res, 'state, 'effect, 'event> with
            member _.Eval env = sub (l.Eval env) (r.Eval env)
        override this.ToString() =
            sprintf "SubExpr(%s, %s)" (l.ToString()) (r.ToString())

    type MulExpr<'l, 'r, 'res, 'state, 'effect, 'event>
        (l: IExpr<'l,'state, 'effect, 'event>, r: IExpr<'r, 'state, 'effect, 'event>, mul: 'l -> 'r -> 'res) =
        interface IExpr<'res, 'state, 'effect, 'event> with
            member _.Eval env = mul (l.Eval env) (r.Eval env)
        override this.ToString() =
            sprintf "MulExpr(%s, %s)" (l.ToString()) (r.ToString())

    type DivExpr<'l, 'r, 'res, 'state, 'effect, 'event>
        (l: IExpr<'l, 'state, 'effect, 'event>, r: IExpr<'r, 'state, 'effect, 'event>, div: 'l -> 'r -> 'res) =
        interface IExpr<'res, 'state, 'effect, 'event> with
            member _.Eval env = div (l.Eval env) (r.Eval env)
        override this.ToString() =
            sprintf "DivExpr(%s, %s)" (l.ToString()) (r.ToString())

    type ModExpr<'state, 'effect, 'event>
        (l: IExpr<int, 'state, 'effect, 'event>, r: IExpr<int, 'state, 'effect, 'event>) =
        interface IExpr<int, 'state, 'effect, 'event> with
            member _.Eval env = l.Eval env % r.Eval env
        override this.ToString() =
            sprintf "ModExpr(%s, %s)" (l.ToString()) (r.ToString())

    type AbsExpr<'a, 'state, 'effect, 'event>
        (e: IExpr<'a, 'state, 'effect, 'event>, abs: 'a -> 'a) =
        interface IExpr<'a, 'state, 'effect, 'event> with
            member _.Eval env = abs (e.Eval env)
        override this.ToString() =
            sprintf "AbsExpr(%s)" (e.ToString())

    type NegExpr<'a, 'state, 'effect, 'event>
        (e: IExpr<'a, 'state, 'effect, 'event>, neg: 'a -> 'a) =
        interface IExpr<'a, 'state, 'effect, 'event> with
            member _.Eval env = neg (e.Eval env)
        override this.ToString() =
            sprintf "NegExpr(%s)" (e.ToString())

    (*
        Comparison Expressions
    *)

    type EqualExpr<'a, 'state, 'effect, 'event when 'a : comparison>
        (l: IExpr<'a, 'state, 'effect, 'event>, r: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = (l.Eval env = r.Eval env)

    type NotEqualExpr<'a, 'state, 'effect, 'event when 'a : comparison>
        (l: IExpr<'a, 'state, 'effect, 'event>, r: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = not (l.Eval env = r.Eval env)

    type GreaterThanExpr<'a, 'state, 'effect, 'event when 'a : comparison>
        (l: IExpr<'a, 'state, 'effect, 'event>, r: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = (l.Eval env > r.Eval env)

    type LessThanExpr<'a, 'state, 'effect, 'event when 'a : comparison>
        (l: IExpr<'a, 'state, 'effect, 'event>, r: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = (l.Eval env < r.Eval env)

    type GreaterOrEqualExpr<'a, 'state, 'effect, 'event when 'a : comparison>
        (l: IExpr<'a, 'state, 'effect, 'event>, r: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = (l.Eval env >= r.Eval env)

    type LessOrEqualExpr<'a, 'state, 'effect, 'event when 'a : comparison>
        (l: IExpr<'a, 'state, 'effect, 'event>, r: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<bool, 'state, 'effect, 'event> with
            member _.Eval env = (l.Eval env <= r.Eval env)

    (*
        Conditional Expressions
    *)

    type IfExpr<'r, 'state, 'effect, 'event>
        (c: IExpr<bool, 'state, 'effect, 'event>, t: IExpr<'r, 'state, 'effect, 'event>, e: IExpr<'r, 'state, 'effect, 'event>) =
        interface IExpr<'r, 'state, 'effect, 'event> with
            member _.Eval env = if c.Eval env then t.Eval env else e.Eval env

    (*
        Lambda
    *)

    type LambdaExpr<'a, 'b, 'state, 'effect, 'event>
        (f: 'a -> 'b) =
        interface IExpr<'a -> 'b, 'state, 'effect, 'event> with
            member _.Eval _ = f

    type ApplyExpr<'a, 'b, 'state, 'effect, 'event>
        (f: IExpr<'a -> 'b, 'state, 'effect, 'event>, arg: IExpr<'a, 'state, 'effect, 'event>) =
        interface IExpr<'b, 'state, 'effect, 'event> with
            member _.Eval env = (f.Eval env) (arg.Eval env)

    (*
        Variables
    *)

    type VarExpr<'a, 'state, 'effect, 'event>
        (name: string) =
        interface IExpr<'a, 'state, 'effect, 'event> with
            member _.Eval env = Env.get name env |> unbox<'a>