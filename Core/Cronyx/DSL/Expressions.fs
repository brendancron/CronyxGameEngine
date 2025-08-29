namespace Cronyx

module Expressions = 
    type IExpr<'a> =
        abstract member Eval : Env -> 'a

    (*
        Utility eval expression to allow for simpler calling of expression evaluations
    *)

    let eval (expr: 'a IExpr) (env: Env) : 'a =
        expr.Eval (env)

    (*
        Primary Type definitions go here
    *)

    type BoolExpr (b: bool) = 
        interface IExpr<bool> with
            member _.Eval env = b

    type IntExpr (i: int) = 
        interface IExpr<int> with
            member _.Eval env = i

    type StringExpr (str: string) = 
        interface IExpr<string> with
            member _.Eval env = str

    type FloatExpr (f: float) = 
        interface IExpr<float> with
            member _.Eval env = f

    (*
        Logic Operators
    *)

    type AndExpr (l: IExpr<bool>, r: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval env = if l.Eval env then r.Eval env else false
    
    type OrExpr (l: IExpr<bool>, r: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval env = if l.Eval env then false else r.Eval env

    type XorExpr (l: IExpr<bool>, r: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval env = l.Eval env <> r.Eval env

    type NotExpr (e: IExpr<bool>) = 
        interface IExpr<bool> with
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

    type AddExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, add: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval env = add (l.Eval env) (r.Eval env)

    type SubExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, sub: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval env = sub (l.Eval env) (r.Eval env)

    type MulExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, mul: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval env = mul (l.Eval env) (r.Eval env)

    type DivExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, div: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval env = div (l.Eval env) (r.Eval env)

    type ModExpr(l: IExpr<int>, r: IExpr<int>) =
        interface IExpr<int> with
            member _.Eval env = l.Eval env % r.Eval env

    type AbsExpr<'a>(e: IExpr<'a>, abs: 'a -> 'a) =
        interface IExpr<'a> with
            member _.Eval env = abs (e.Eval env)

    type NegExpr<'a>(e: IExpr<'a>, neg: 'a -> 'a) =
        interface IExpr<'a> with
            member _.Eval env = neg (e.Eval env)

    (*
        Comparison Expressions
    *)

    type EqualExpr<'a when 'a : equality>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval env = (l.Eval env = r.Eval env)

    type NotEqualExpr<'a when 'a : equality>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval env = not (l.Eval env = r.Eval env)

    type GreaterThanExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval env = (l.Eval env > r.Eval env)

    type LessThanExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval env = (l.Eval env < r.Eval env)

    type GreaterOrEqualExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval env = (l.Eval env >= r.Eval env)

    type LessOrEqualExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval env = (l.Eval env <= r.Eval env)

    (*
        Conditional Expressions
    *)

    type TernaryExpr<'r>(c: IExpr<bool>, t: IExpr<'r>, e: IExpr<'r>) =
        interface IExpr<'r> with
            member _.Eval env = if c.Eval env then t.Eval env else e.Eval env

    (*
        Lambda
    *)

    type LambdaExpr<'a,'b>(f: 'a -> 'b) =
        interface IExpr<'a -> 'b> with
            member _.Eval env = f

    type ApplyExpr<'a,'b>(f: IExpr<'a -> 'b>, arg: IExpr<'a>) =
        interface IExpr<'b> with
            member _.Eval env = (f.Eval env) (arg.Eval env)

    (*
        Variables
    *)

    type VarExpr<'a>(name: string) =
        interface IExpr<'a> with
            member _.Eval env = Env.get name env |> unbox<'a>
