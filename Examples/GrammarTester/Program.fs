open Cronyx.Parsing

let source = "var str = \"Hello World!!\";"

printf "%A" (Lexer.scan source)