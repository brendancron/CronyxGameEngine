namespace Cronyx.Evaluation.Components

open System
open Cronyx.Evaluation.Models.Tokens
open Cronyx.Evaluation.Models.Expressions

module Parser =

    // ---------- Core types ----------
    exception ParseError of string * int

    type State = {
        tokens: Token array
        pos: int
    }

    type Parser<'a> = State -> Result<'a * State, string>

    let inline ok x s   = Ok (x, s)
    let inline fail m _ = Error m

    // ---------- CE builder ----------
    let bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
        fun s -> match p s with Ok (a, s') -> f a s' | Error e -> Error e

    type ParserBuilder() =
        member _.Return x      : Parser<_> = fun s -> ok x s
        member _.Bind (p, f)   : Parser<_> = bind p f
        member _.ReturnFrom p  : Parser<_> = p
        member _.Zero()        : Parser<_> = fun s -> ok () s
        member _.Delay f       : Parser<_> = fun s -> f() s

    let parser = ParserBuilder()

    // ---------- Primitives ----------
    let run (p: Parser<'a>) (tokens: Token list) =
        p { tokens = List.toArray tokens; pos = 0 }

    let peek : Parser<Token option> =
        fun s ->
            if s.pos < s.tokens.Length then ok (Some s.tokens.[s.pos]) s
            else ok None s

    let advance : Parser<Token> =
        fun s ->
            if s.pos < s.tokens.Length then
                let t = s.tokens.[s.pos]
                ok t { s with pos = s.pos + 1 }
            else Error "Unexpected EOF"

    let consume (tt: TokenType) : Parser<Token> =
        parser {
            let! t = advance
            if t.TokenType = tt then return t
            else return! fail (sprintf "Expected %A, got %A on line %d" tt t.TokenType t.Line)
        }

    let peekType : Parser<TokenType option> =
        parser {
            let! t = peek
            return t |> Option.map (fun x -> x.TokenType)
        }

    let lookaheadType (k:int) : Parser<TokenType option> =
        fun s ->
            let i = s.pos + k
            if i < s.tokens.Length then ok (Some s.tokens.[i].TokenType) s
            else ok None s

    let optional (p: Parser<'a>) : Parser<'a option> =
        fun s ->
            match p s with
            | Ok (v, s') -> Ok (Some v, s')
            | Error _    -> Ok (None, s)

    let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
        fun env ->
            try
                p1 env
            with _ ->
                p2 env

    // ---------- Expression parsers (stubs; you implement) ----------
    let rec parseExpr : Parser<Expr> =
        parser { return! fail "TODO: parseExpr" }

    and parseLet : Parser<Expr> =
        parser { return! fail "TODO: parseLet" }

    and parseApplication : Parser<Expr> =
        parser { return! fail "TODO: parseApplication" }

    and parseSum : Parser<Expr> =
        parser { 
            let! first = parseProduct
            let rec loop lhs =
                parser {
                    let! tt = peekType
                    match tt with
                        | Some PLUS ->
                            let! _ = consume PLUS
                            let! rhs = parseProduct
                            let newLhs = BinaryExpression (Add, lhs, rhs)
                            return! loop newLhs
                        | Some MINUS ->
                            let! _ = consume MINUS
                            let! rhs = parseProduct
                            let newLhs = BinaryExpression (Sub, lhs, rhs)
                            return! loop newLhs
                        | _ -> return lhs
                }
            return! loop first
        }

    and parseProduct : Parser<Expr> =
        parser { 
            let! first = parsePrimary
            let rec loop lhs =
                parser {
                    let! tt = peekType
                    match tt with
                        | Some STAR ->
                            let! _ = consume STAR
                            let! rhs = parsePrimary
                            let newLhs = BinaryExpression (Mul, lhs, rhs)
                            return! loop newLhs
                        | Some SLASH ->
                            let! _ = consume SLASH
                            let! rhs = parsePrimary
                            let newLhs = BinaryExpression (Div, lhs, rhs)
                            return! loop newLhs
                        | _ -> return lhs
                }
            return! loop first
        }

    and parsePrimary : Parser<Expr> =
        parser {
            let! tokenType = peekType
            match tokenType with
            | Some INT ->
                let! t = consume INT
                return ConstExpression (IntConst (int t.Lexeme))
            | Some FLOAT ->
                let! t = consume FLOAT
                return ConstExpression (FloatConst (float t.Lexeme))
            | Some TRUE ->
                let! _ = consume TRUE
                return ConstExpression (BoolConst true)
            | Some FALSE ->
                let! _ = consume TRUE
                return ConstExpression (BoolConst false)
            | Some STRING ->
                let! t = consume STRING
                return ConstExpression (StringConst t.Lexeme)
            | Some IDENTIFIER ->
                let! t = consume IDENTIFIER
                let name = t.Lexeme
                return VariableExpression name
            | _ -> return! fail "Expected primary expression"
            
        }

    and parseLambda : Parser<Expr> =
      parser { return! fail "TODO: parseLambda" }

    // ---------- Public API ----------
    let parse (tokens: Token list) : Result<Expr, string> =
      run parseSum tokens |> Result.map (fun (e, _) -> e)
