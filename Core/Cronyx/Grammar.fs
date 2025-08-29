namespace Cronyx

module Grammar = 
    type IExpr<'a> =
        abstract member Eval : unit -> 'a

    (*
        Utility eval expression to allow for simpler calling of expression evaluations
    *)

    let eval (expr: 'a IExpr) : 'a =
        expr.Eval ()

    (*
        Primary Type definitions go here
    *)

    type BoolExpr (b: bool) = 
        interface IExpr<bool> with
            member _.Eval () = b

    type IntExpr (i: int) = 
        interface IExpr<int> with
            member _.Eval () = i

    type StringExpr (str: string) = 
        interface IExpr<string> with
            member _.Eval () = str

    type FloatExpr (f: float) = 
        interface IExpr<float> with
            member _.Eval () = f

    (*
        Logic Operators
    *)

    type AndExpr (l: IExpr<bool>, r: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval () = if l.Eval () then r.Eval () else false
    
    type OrExpr (l: IExpr<bool>, r: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval () = if l.Eval () then false else r.Eval ()

    type XorExpr (l: IExpr<bool>, r: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval () = l.Eval () <> r.Eval ()

    type NotExpr (e: IExpr<bool>) = 
        interface IExpr<bool> with
            member _.Eval () = not (e.Eval ())

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
            member _.Eval() = add (l.Eval()) (r.Eval())

    type SubExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, sub: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval() = sub (l.Eval()) (r.Eval())

    type MulExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, mul: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval() = mul (l.Eval()) (r.Eval())

    type DivExpr<'l,'r,'res>(l: IExpr<'l>, r: IExpr<'r>, div: 'l -> 'r -> 'res) =
        interface IExpr<'res> with
            member _.Eval() = div (l.Eval()) (r.Eval())

    type ModExpr(l: IExpr<int>, r: IExpr<int>) =
        interface IExpr<int> with
            member _.Eval() = l.Eval() % r.Eval()

    type AbsExpr<'a>(e: IExpr<'a>, abs: 'a -> 'a) =
        interface IExpr<'a> with
            member _.Eval() = abs (e.Eval())

    type NegExpr<'a>(e: IExpr<'a>, neg: 'a -> 'a) =
        interface IExpr<'a> with
            member _.Eval() = neg (e.Eval())

    (*
        Comparison Expressions
    *)

    type EqualExpr<'a when 'a : equality>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval() = (l.Eval() = r.Eval())

    type NotEqualExpr<'a when 'a : equality>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval() = not (l.Eval() = r.Eval())

    type GreaterThanExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval() = (l.Eval() > r.Eval())

    type LessThanExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval() = (l.Eval() < r.Eval())

    type GreaterOrEqualExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval() = (l.Eval() >= r.Eval())

    type LessOrEqualExpr<'a when 'a : comparison>(l: IExpr<'a>, r: IExpr<'a>) =
        interface IExpr<bool> with
            member _.Eval() = (l.Eval() <= r.Eval())

    (*
        Conditional Expressions
    *)

    type TernaryExpr<'r>(c: IExpr<bool>, t: IExpr<'r>, e: IExpr<'r>) =
        interface IExpr<'r> with
            member _.Eval () = if c.Eval() then t.Eval() else e.Eval()

    (*
        Lambda
    *)

    type LambdaExpr<'a,'b>(f: 'a -> 'b) =
        interface IExpr<'a -> 'b> with
            member _.Eval() = f

    type ApplyExpr<'a,'b>(f: IExpr<'a -> 'b>, arg: IExpr<'a>) =
        interface IExpr<'b> with
            member _.Eval() = (f.Eval()) (arg.Eval())