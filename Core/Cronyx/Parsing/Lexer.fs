namespace Cronyx.Parsing

module Lexer = 
    
    open Tokens
    open System

    let private keywords =
        dict [
            "and", AND
            "else", ELSE
            "false", FALSE
            "for", FOR
            "if", IF
            "nil", NIL
            "or", OR
            "return", RETURN
            "true", TRUE
            "var", VAR
            "while", WHILE
        ]

    let private makeToken tokenType lexeme line =
        { TokenType = tokenType; Lexeme = lexeme; Line = line }

    let rec private scanTokens (source: string) (i: int) (line: int) : Token list =
        if i >= source.Length then
            [ makeToken EOF "" line ]
        else
            let c = source.[i]
            match c with
            | '(' -> makeToken LPAREN "(" line :: scanTokens source (i+1) line
            | ')' -> makeToken RPAREN ")" line :: scanTokens source (i+1) line
            | '{' -> makeToken LBRACE "{" line :: scanTokens source (i+1) line
            | '}' -> makeToken RBRACE "}" line :: scanTokens source (i+1) line
            | ',' -> makeToken COMMA "," line :: scanTokens source (i+1) line
            | '.' -> makeToken DOT "." line :: scanTokens source (i+1) line
            | '-' -> makeToken MINUS "-" line :: scanTokens source (i+1) line
            | '+' -> makeToken PLUS "+" line :: scanTokens source (i+1) line
            | ';' -> makeToken SEMICOLON ";" line :: scanTokens source (i+1) line
            | '!' when i+1 < source.Length && source.[i+1] = '=' ->
                makeToken BANG_EQUAL "!=" line :: scanTokens source (i+2) line
            | '!' -> makeToken BANG "!" line :: scanTokens source (i+1) line
            | '=' when i+1 < source.Length && source.[i+1] = '=' ->
                makeToken EQUAL_EQUAL "==" line :: scanTokens source (i+2) line
            | '=' -> makeToken EQUAL "=" line :: scanTokens source (i+1) line
            | '<' when i+1 < source.Length && source.[i+1] = '=' ->
                makeToken LESS_EQUAL "<=" line :: scanTokens source (i+2) line
            | '<' -> makeToken LESS "<" line :: scanTokens source (i+1) line
            | '>' when i+1 < source.Length && source.[i+1] = '=' ->
                makeToken GREATER_EQUAL ">=" line :: scanTokens source (i+2) line
            | '>' -> makeToken GREATER ">" line :: scanTokens source (i+1) line
            | ' ' | '\r' | '\t' ->
                scanTokens source (i+1) line
            | '\n' ->
                scanTokens source (i+1) (line+1)
            | '"' ->
                let start = i+1
                let rec loop j line =
                    if j >= source.Length then
                        failwithf "Unterminated string at line %d" line
                    elif source.[j] = '"' then
                        let strVal = source.Substring(start, j-start)
                        makeToken STRING strVal line :: scanTokens source (j+1) line
                    else
                        loop (j+1) (if source.[j] = '\n' then line+1 else line)
                loop start line
            | d when Char.IsDigit d ->
                let start = i
                let rec loop j =
                    if j < source.Length && Char.IsDigit source.[j] then loop (j+1)
                    else j
                let j = loop i
                let numVal = source.Substring(start, j-start)
                makeToken NUMBER numVal line :: scanTokens source j line
            | a when Char.IsLetter a || a = '_' ->
                let start = i
                let rec loop j =
                    if j < source.Length && (Char.IsLetterOrDigit source.[j] || source.[j] = '_') then loop (j+1)
                    else j
                let j = loop i
                let text = source.Substring(start, j-start)
                let tokenType =
                    match keywords.TryGetValue text with
                    | true, kw -> kw
                    | _ -> IDENTIFIER
                makeToken tokenType text line :: scanTokens source j line
            | _ ->
                failwithf "Unexpected character '%c' at line %d" c line

    let scan (source: string) : Token list =
        scanTokens source 0 1