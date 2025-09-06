namespace Cronyx.Parsing

module Tokens = 
    type TokenType =
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE
        | COMMA
        | DOT
        | MINUS
        | PLUS
        | SEMICOLON
        | BANG
        | BANG_EQUAL
        | EQUAL
        | EQUAL_EQUAL
        | GREATER
        | GREATER_EQUAL
        | LESS
        | LESS_EQUAL
        | IDENTIFIER
        | STRING
        | NUMBER
        | AND
        | ELSE
        | FALSE
        | FOR
        | IF
        | NIL
        | OR
        | RETURN
        | TRUE
        | VAR
        | WHILE
        | EOF

    type Token = {
        TokenType: TokenType
        Lexeme: string
        Line: int
    }