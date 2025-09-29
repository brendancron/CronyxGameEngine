module GrammarTester.Tests

open Expecto
open Cronyx.Evaluation.Components.Parser
open Cronyx.Evaluation.Components.Lexer
open Cronyx.Evaluation.Models.Tokens
open Cronyx.Evaluation.Models.Expressions

type Tests = 
    
    [<Tests>]
    static member ParserTests =
        testList "parser" [
            testCase "parse identifier" <| fun _ ->
                let source = "\"Hello World!\""
                let tokens = scan source
                let expr = parse tokens
                match expr with
                | Error e -> failwith e
                | Ok expr ->
                    // NOTE: You are testing against "Hello World", but your source is "Hello World!"
                    Expect.equal expr (ConstExpression (StringConst "Hello World!")) "Should parse string const"
        ]