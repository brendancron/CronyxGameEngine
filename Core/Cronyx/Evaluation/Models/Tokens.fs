namespace Cronyx.Evaluation.Models

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
        | STAR
        | SLASH
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
        | INT
        | FLOAT
        | AND
        | ELSE
        | FALSE
        | FOR
        | IF
        | NIL
        | OR
        | RETURN
        | TRUE
        | WHILE
        | EOF

    type Token = {
        TokenType: TokenType
        Lexeme: string
        Line: int
    }