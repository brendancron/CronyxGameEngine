namespace Cronyx.Parsing

open Cronyx.Evaluation.Grammar
open Cronyx.Evaluation.Expressions
open Cronyx.Evaluation.Statements
open Cronyx.Parsing.Tokens

module Parser =

    type ParseEnv<'s,'e,'ev> = {
        Tokens   : Token list
        EffectResolver : EffectResolver<'s,'e,'ev> }

    // The monadic type
    type Parser<'a,'s,'e,'ev> = ParseEnv<'s,'e,'ev> -> ('a * ParseEnv<'s,'e,'ev> * string list)

    // The 'return' function
    let result (x: 'a) : Parser<'a,'s,'e,'ev> =
        fun env -> (x, env, [])

    // The 'bind' function
    let bind (p: Parser<'a,'s,'e,'ev>) (f: 'a -> Parser<'b,'s,'e,'ev>) : Parser<'b,'s,'e,'ev> =
        fun env ->
            let (value, env1, trace1) = p env
            let (result, env2, trace2) = f value env1
            (result, env2, trace1 @ trace2)

    type ParserBuilder() =
        member _.Return(x) = result x
        member _.Bind(p, f) = bind p f
        member _.ReturnFrom(p: Parser<'a,'s,'e,'ev>) = p
        member _.Zero() = fun _ -> failwith "Parser.Zero"

    let parser = ParserBuilder()

    let log (msg: string) : Parser<unit,'s,'e,'ev> =
        fun tokens -> (), tokens, [msg]

    let matchToken (expected: TokenType) : Parser<Token,'s,'e,'ev> =
        fun env ->
            match env.Tokens with
            | t::ts when t.TokenType = expected ->
                t, ts, []
            | t::_ ->
                failwithf "Unexpected token %A at line %d. Expected %A."
                    t.TokenType t.Line expected
            | [] ->
                failwithf "Unexpected end of input. Expected %A." expected

    let peekToken : Parser<Token option,'s,'e,'ev> =
        fun env ->
            match env.Tokens with
            | [] -> (None, env, [])
            | t::_ -> (Some t, env, [])
    
    let getEffectResolver : Parser<EffectResolver,'s,'e,'ev> =
        fun env -> (env.EffectResolver, env, [])

    let rec many (p: Parser<'a,'s,'e,'ev>) : Parser<'a list,'s,'e,'ev> =
        fun env ->
            try
                let (x, env1, trace1) = p env
                let (xs, env2, trace2) = many p env1
                (x :: xs, env2, trace1 @ trace2)
            with _ ->
                ([], env, [])

    let many1 (p: Parser<'a,'s,'e,'ev>) : Parser<'a list,'s,'e,'ev> =
        fun env ->
            let (x, env1, trace1) = p env
            let (xs, env2, trace2) = many p env1
            (x :: xs, env2, trace1 @ trace2)

    let (<|>) (p1: Parser<'a,'s,'e,'ev>) (p2: Parser<'a,'s,'e,'ev>) : Parser<'a,'s,'e,'ev> =
        fun env ->
            try
                p1 env
            with _ ->
                p2 env

    let rec parseProgram<'s,'e,'ev> : Parser<IStmt<'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! (stmts: IStmt<'s,'e,'ev> list) = many (parseCallStmt<'s,'e,'ev> <|> parseExprStmt<'s,'e,'ev>)
            let! _ = matchToken TokenType.EOF
            return BlockStmt(stmts) :> IStmt<'s,'e,'ev>
        }

    and parseCallStmt<'s,'e,'ev> : Parser<IStmt<'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! _ = matchToken TokenType.IDENTIFIER
            let! _ = matchToken TokenType.LPAREN
            let! (expr : IExpr<int,'s,'e,'ev>) = parseExpr<'s,'e,'ev>
            let! _ = matchToken TokenType.RPAREN
            let! _ = matchToken TokenType.SEMICOLON
            return FnStmt(expr, (printf "%d")) :> IStmt<'s,'e,'ev>
        }

    and parseExprStmt<'s,'e,'ev> : Parser<IStmt<'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! (expr : IExpr<int,'s,'e,'ev>) = parseExpr<'s,'e,'ev>
            let! _ = matchToken TokenType.SEMICOLON
            return ExprStmt(expr) :> IStmt<'s,'e,'ev>
        }

    and parseExpr<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! expr = parseSum<'s,'e,'ev>
            return expr
        }

    and parseArgs<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev> list,'s,'e,'ev> =
        parser {
            let! first = parseExpr<'s,'e,'ev>

            let rec loop acc =
                parser {
                    let! tokOpt = peekToken
                    match tokOpt with
                    | Some { TokenType = TokenType.COMMA } ->
                        let! _ = matchToken TokenType.COMMA
                        let! next = parseExpr<'s,'e,'ev>
                        return! loop (next :: acc)

                    | Some { TokenType = TokenType.RPAREN } ->
                        return List.rev acc

                    | Some t ->
                        failwithf "Unexpected token in arg list: %A" t.TokenType

                    | None ->
                        failwith "Unexpected EOF in arg list"
                }

            return! loop [first]
        }

    and parseSum<'s,'e,'ev>
        : Parser<IExpr<int,'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! first = parseFactor<'s,'e,'ev>

            let rec loop lhs =
                parser {
                    match! peekToken with
                    | Some t when t.TokenType = Tokens.TokenType.PLUS ->
                        let! _ = matchToken TokenType.PLUS
                        do! log "Matched + in sum"
                        let! rhs = parseFactor<'s,'e,'ev>
                        let lhs' = AddExpr(lhs, rhs, (+)) :> IExpr<_,_,_,_>
                        return! loop lhs'
                    | _ ->
                        return lhs
                }

            return! loop first
        }

    and parseFactor<'s,'e,'ev>
        : Parser<IExpr<int,'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! first = parsePrimary<'s,'e,'ev>

            let rec loop lhs : Parser<IExpr<int,'s,'e,'ev>,'s,'e,'ev> =
                parser {
                    match! peekToken with
                    | Some t when t.TokenType = TokenType.STAR ->
                        let! _ = matchToken TokenType.STAR
                        do! log "Matched * in factor"
                        let! rhs = parsePrimary<'s,'e,'ev>
                        let lhs' = MulExpr(lhs, rhs, (*)) :> IExpr<_,_,_,_>
                        return! loop lhs'
                    | _ ->
                        return lhs
                }

            return! loop first
        }

    and parseUnary<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! tokOpt = peekToken
            match tokOpt with
            | Some t when t.TokenType = TokenType.MINUS ->
                // consume the minus
                let! _ = matchToken TokenType.MINUS
                do! log "Matched - in unary"
                // parse the right-hand side
                let! expr = parseUnary<'s,'e,'ev>
                return NegExpr(expr, (fun x -> -x)) :> IExpr<_,_,_,_>
            | _ ->
                // otherwise delegate to primary
                return! parsePrimary<'s,'e,'ev>
        }

    and parsePrimary<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev>,'s,'e,'ev> =
        parser {
            let! tokOpt = peekToken
            match tokOpt with
            | Some { TokenType = TokenType.NUMBER; Lexeme = lex } ->
                let! t = matchToken Tokens.TokenType.NUMBER
                do! log "Matched number in primary"
                return IntExpr<'s,'e,'ev>(int t.Lexeme) :> IExpr<_,_,_,_>

            | Some { TokenType = TokenType.IDENTIFIER; Lexeme = name } ->
                return! parseIdentifier

            | Some { TokenType = TokenType.LPAREN } ->
                let! _ = matchToken Tokens.TokenType.LPAREN
                do! log "Matched ( in primary"
                let! expr = parseExpr<'s,'e,'ev>
                let! _ = matchToken Tokens.TokenType.RPAREN
                do! log "Matched ) in primary"
                return expr

            | Some t ->
                failwithf "Unexpected token in primary: %A at line %d" t.TokenType t.Line

            | None ->
                failwith "Unexpected end of input in primary"
        }

    and parseEffect<'s,'e,'ev> : Parser<EffectExpr<'e, 's, 'e, 'ev>,'s,'e,'ev> =
        parser {
            let! identTok = matchToken TokenType.IDENTIFIER
            let ident = identTok.Lexeme

            let! _ = matchToken TokenType.LPAREN
            let! args = parseArgs<'s,'e,'ev>
            let! _ = matchToken TokenType.RPAREN

            let! effectResolver = getEffectResolver

            let call = { Name = ident; Args = args }

            return EffectExpr<'s,'e,'ev>(call, resolver) :> IExpr<'e,'s,'e,'ev>
        }

    and parseIdentifier<'s,'e,'ev> : Parser<IExpr<int, 's, 'e, 'ev>,'s,'e,'ev> =
        parser {
            let! t = matchToken TokenType.IDENTIFIER
            do! log "Matched identifier in primary"
            return VarExpr<int,'s,'e,'ev>(t.Lexeme)
        }

    let parse<'s,'e,'ev> (tokens: Token list) (effectResolver: EffectResolver) =
        let env = {
            Tokens = tokens
            EffectResolver = effectResolver
        }
        parseProgram<'s,'e,'ev> env
        