namespace Cronyx.Parsing

open Cronyx.Core
open Cronyx.Expressions

module Parser =
    let private peek = function
        | [] -> None
        | t::_ -> Some t

    let private advance = function
        | [] -> failwith "Unexpected end of input"
        | _::rest -> rest

    let private expect (tt: Tokens.TokenType) (err: string) (ts: Tokens.Token list) =
        match ts with
        | t::rest when t.TokenType = tt -> rest, t
        | _ -> failwith err

    let private matchOne (tt: Tokens.TokenType) (ts: Tokens.Token list) =
        match ts with
        | t::rest when t.TokenType = tt -> Some (rest, t)
        | _ -> None

    let rec parseExpr<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
        (ts: Tokens.Token list)
        : IExpr<int,'eff,'event,'state> * Tokens.Token list =
        parseAddition ts

    and private parseAddition<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
        (ts: Tokens.Token list)
        : IExpr<int,'eff,'event,'state> * Tokens.Token list =
        let rec loop (lhs: IExpr<int,'eff,'event,'state>) (ts: Tokens.Token list) =
            match peek ts with
            | Some { Tokens.TokenType = Tokens.TokenType.PLUS } ->
                let ts1 = advance ts
                let rhs, ts2 = parseUnary ts1
                let lhs' =
                    AddExpr<int, int, int,'eff,'event,'state>(lhs, rhs, (+)) :> IExpr<_,_,_,_>
                loop lhs' ts2
            | Some { Tokens.TokenType = Tokens.TokenType.MINUS } ->
                let ts1 = advance ts
                let rhs, ts2 = parseUnary ts1
                let lhs' =
                    SubExpr<int, int, int,'eff,'event,'state>(lhs, rhs, (-)) :> IExpr<_,_,_,_>
                loop lhs' ts2
            | _ -> lhs, ts
        let first, rest = parseUnary ts
        loop first rest

    and private parseUnary<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
          (ts: Tokens.Token list)
          : IExpr<int,'eff,'event,'state> * Tokens.Token list =
        match peek ts with
        | Some { Tokens.TokenType = Tokens.TokenType.MINUS } ->
            let ts1 = advance ts
            let expr, ts2 = parseUnary ts1
            (NegExpr<int,'eff,'event,'state>(expr, (fun x -> -x)) :> IExpr<_,_,_,_>), ts2
        | _ ->
            parsePrimary ts

    and private parsePrimary<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
        (ts: Tokens.Token list)
        : IExpr<int,'eff,'event,'state> * Tokens.Token list =
        match ts with
        | { Tokens.TokenType = Tokens.TokenType.NUMBER; Lexeme = lex } :: rest ->
            // NUMBER -> IntExpr
            (IntExpr<'eff,'event,'state>(int lex) :> IExpr<_,_,_,_>), rest
        | { Tokens.TokenType = Tokens.TokenType.IDENTIFIER; Lexeme = name } :: rest ->
            // IDENTIFIER -> assume int variable for arithmetic
            (VarExpr<int,'eff,'event,'state>(name) :> IExpr<_,_,_,_>), rest
        | { Tokens.TokenType = Tokens.TokenType.LPAREN } :: rest ->
            let expr, rest' = parseExpr<'eff,'event,'state> rest
            let rest'', _ = expect Tokens.TokenType.RPAREN "Expected ')' after expression" rest'
            expr, rest''
        | t::_ ->
            failwithf "Unexpected token in primary: %A at line %d" t.TokenType t.Line
        | [] ->
            failwith "Unexpected end of input in primary"

    let parseArithmetic<'eff,'event,'state when 'state :> IGameState<'eff,'event,'state>>
        (tokens: Tokens.Token list)
        : IExpr<int,'eff,'event,'state> * Tokens.Token list =
        parseExpr tokens