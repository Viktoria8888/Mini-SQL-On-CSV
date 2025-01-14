open Ast
let parse (s : string) : command =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.tokenize lexbuf in
  ast