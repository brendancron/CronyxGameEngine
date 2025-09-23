namespace Cronyx.Parsing

open Cronyx.Evaluation.Grammar
open Cronyx.Evaluation.Expressions
open Cronyx.Evaluation.Statements
open Cronyx.Parsing.Tokens

module Parser =
    // The monadic type
    type Parser<'a> = Token list -> ('a * Token list * string list)

    // The 'return' function
    let result (x: 'a) : Parser<'a> =
        fun tokens -> x, tokens, []

    // The 'bind' function
    let bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
        fun tokens ->
            let (value, restTokens, trace1) = p tokens
            let (result, finalTokens, trace2) = (f value) restTokens
            result, finalTokens, trace1 @ trace2

    type ParserBuilder() =
        member _.Return(x) = result x
        member _.Bind(p, f) = bind p f
        member _.ReturnFrom(p: Parser<'a>) = p
        member _.Zero() : Parser<'a> = fun _ -> failwith "Parser.Zero: no result"

    let parser = ParserBuilder()

    let log (msg: string) : Parser<unit> =
        fun tokens -> (), tokens, [msg]

    let matchToken (expected: TokenType) : Parser<Token> =
        fun tokens ->
            match tokens with
            | [] ->
                failwithf "Unexpected end of input. Expected %A." expected
            | t::ts when t.TokenType = expected ->
                t, ts, []
            | t::_ ->
                failwithf "Unexpected token %A at line %d. Expected %A."
                    t.TokenType t.Line expected

    let peekToken : Parser<Token option> =
        fun tokens ->
            match tokens with
            | []      -> None, tokens, []
            | t :: _  -> Some t, tokens, []
    
    let rec parseCallStmt<'s,'e,'ev> : Parser<IStmt<'s,'e,'ev>> =
        parser {
            let! _ = matchToken TokenType.IDENTIFIER
            let! _ = matchToken TokenType.LPAREN
            let! (expr : IExpr<int,'s,'e,'ev>) = parseExpr<'s,'e,'ev>
            let! _ = matchToken TokenType.RPAREN
            let! _ = matchToken TokenType.SEMICOLON
            return FnStmt(expr, (printf "%d")) :> IStmt<'s,'e,'ev>
        }

    and parseExprStmt<'s,'e,'ev> : Parser<IStmt<'s,'e,'ev>> =
        parser {
            let! (expr : IExpr<int,'s,'e,'ev>) = parseExpr<'s,'e,'ev>
            let! _ = matchToken TokenType.SEMICOLON
            return ExprStmt(expr) :> IStmt<'s,'e,'ev>
        }

    and parseExpr<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev>> =
        parser {
            let! factor = parseSum<'s,'e,'ev>
            return factor
        }

    and parseSum<'s,'e,'ev>
        : Parser<IExpr<int,'s,'e,'ev>> =
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
        : Parser<IExpr<int,'s,'e,'ev>> =
        parser {
            let! first = parsePrimary<'s,'e,'ev>

            let rec loop lhs : Parser<IExpr<int,'s,'e,'ev>> =
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

    and parseUnary<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev>> =
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

    and parsePrimary<'s,'e,'ev> : Parser<IExpr<int,'s,'e,'ev>> =
        parser {
            let! tokOpt = peekToken
            match tokOpt with
            | Some { TokenType = TokenType.NUMBER; Lexeme = lex } ->
                let! t = matchToken Tokens.TokenType.NUMBER
                do! log "Matched number in primary"
                return IntExpr<'s,'e,'ev>(int t.Lexeme) :> IExpr<_,_,_,_>

            | Some { TokenType = TokenType.IDENTIFIER; Lexeme = name } ->
                let! t = matchToken Tokens.TokenType.IDENTIFIER
                do! log "Matched identifier in primary"
                return VarExpr<int,'s,'e,'ev>(t.Lexeme) :> IExpr<_,_,_,_>

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

    let parse<'s,'e,'ev> (tokens: Tokens.Token list) =
        parseCallStmt<'s,'e,'ev> tokens