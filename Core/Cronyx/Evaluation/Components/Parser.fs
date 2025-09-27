namespace Cronyx.Evaluation.Components

open System
open Cronyx.Evaluation.Models.Tokens
open Cronyx.Evaluation.Models.Expressions

module Parser =

    // Simple parse error
    exception ParseError of string * int

    // Internal parser state (convert list -> array for O(1) indexing)
    type private State =
        { tokens: Token array
          mutable pos: int }

    // --- Helpers -------------------------------------------------------------

    let private atEnd (st: State) =
        st.pos >= st.tokens.Length
        || st.tokens.[st.pos].TokenType = TokenType.EOF

    let private peek (st: State) =
        if st.pos < st.tokens.Length then st.tokens.[st.pos]
        else st.tokens.[st.tokens.Length - 1]

    let private lookahead (st: State) (k: int) =
        let i = st.pos + k
        if i < st.tokens.Length then Some st.tokens.[i] else None

    let private check (tt: TokenType) (st: State) =
        not (atEnd st) && ((peek st).TokenType = tt)

    let private advance (st: State) =
        let t = peek st
        st.pos <- st.pos + 1
        t

    let private matchTok (tt: TokenType) (st: State) =
        if check tt st then
            st.pos <- st.pos + 1
            true
        else false

    let private consume (tt: TokenType) (msg: string) (st: State) =
        if matchTok tt st then st.tokens.[st.pos - 1]
        else
            let line =
                if st.pos < st.tokens.Length then st.tokens.[st.pos].Line else -1
            raise (ParseError (msg, line))

    let private startsPrimary (st: State) =
        match lookahead st 0 with
        | Some t ->
            match t.TokenType with
            | TokenType.IDENTIFIER
            | TokenType.INT
            | TokenType.FLOAT
            | TokenType.STRING
            | TokenType.TRUE
            | TokenType.FALSE
            | TokenType.LPAREN -> true
            | _ -> false
        | None -> false

    let private isLambdaAhead (st: State) =
        // ( IDENT ) -> ...
        match lookahead st 0, lookahead st 1, lookahead st 2, lookahead st 3, lookahead st 4 with
        | Some t0, Some t1, Some t2, Some t3, Some t4
            when t0.TokenType = TokenType.LPAREN
              && t1.TokenType = TokenType.IDENTIFIER
              && t2.TokenType = TokenType.RPAREN
              && t3.TokenType = TokenType.MINUS
              && t4.TokenType = TokenType.GREATER -> true
        | _ -> false

    // --- Parsing -------------------------------------------------------------

    let rec private parseExpr (st: State) : Expr =
        parseLet st

    and private parseLet (st: State) : Expr =
        if matchTok TokenType.VAR st then
            let nameTok = consume TokenType.IDENTIFIER "Expect variable name after 'var'." st
            ignore (consume TokenType.EQUAL "Expect '=' after variable name." st)
            let bound = parseExpr st
            ignore (consume TokenType.SEMICOLON "Expect ';' after initializer in let." st)
            let body = parseExpr st
            LetExpression (nameTok.Lexeme, bound, body)
        else
            parseApplication st

    and private parseApplication (st: State) : Expr =
        // left-associative: (((head arg1) arg2) arg3) ...
        let mutable head = parsePrimary st
        while startsPrimary st do
            let arg = parsePrimary st
            head <- ApplicationExpression (head, arg)
        head

    and private parsePrimary (st: State) : Expr =
        if isLambdaAhead st then
            parseLambda st
        else
            match advance st with
            | t when t.TokenType = TokenType.IDENTIFIER ->
                VariableExpression t.Lexeme

            | t when t.TokenType = TokenType.INT ->
                match Int32.TryParse t.Lexeme with
                | true, v -> ConstExpression (IntConst v)
                | _ -> failwithf "bad int: %s" t.Lexeme

            | t when t.TokenType = TokenType.FLOAT ->
                match Double.TryParse(t.Lexeme, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                | true, v -> ConstExpression (FloatConst v)
                | _ -> failwithf "bad float: %s" t.Lexeme

            | t when t.TokenType = TokenType.STRING ->
                let s = t.Lexeme
                let s' =
                    if s.Length >= 2 && s.[0] = '"' && s.[s.Length - 1] = '"' then
                        s.Substring(1, s.Length - 2)
                    else s
                ConstExpression (StringConst s')

            | t when t.TokenType = TokenType.TRUE  -> ConstExpression (BoolConst true)
            | t when t.TokenType = TokenType.FALSE -> ConstExpression (BoolConst false)

            | t when t.TokenType = TokenType.LPAREN ->
                // grouped expression
                let inner = parseExpr st
                ignore (consume TokenType.RPAREN "Expect ')' after expression." st)
                inner

            | t ->
                raise (ParseError (sprintf "Unexpected token %A" t.TokenType, t.Line))

    and private parseLambda (st: State) : Expr =
        // (x) -> body
        ignore (consume TokenType.LPAREN "Expect '(' starting lambda." st)
        let nameTok = consume TokenType.IDENTIFIER "Expect parameter name." st
        ignore (consume TokenType.RPAREN "Expect ')' after parameter." st)
        ignore (consume TokenType.MINUS "Expect '-' of '->'." st)
        ignore (consume TokenType.GREATER "Expect '>' of '->'." st)
        let body = parseExpr st
        AbstractionExpression (nameTok.Lexeme, body)

    // Public API
    let parse (tokens: Token list) : Expr =
        let st = { tokens = tokens |> List.toArray; pos = 0 }
        let expr = parseExpr st
        // optionally require EOF
        match lookahead st 0 with
        | Some t when t.TokenType = TokenType.EOF -> expr
        | Some t ->
            raise (ParseError (sprintf "Expected EOF, found %A" t.TokenType, t.Line))
        | None -> expr
