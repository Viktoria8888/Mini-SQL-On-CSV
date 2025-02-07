{
open Parser
}

rule tokenize = parse
| [' ' '\t' '\n' ] { tokenize lexbuf }
| "SELECT" { tokenize lexbuf}
| "READ"              { READ }
| "DELETE"            { DELETE }
| "WHERE"             { WHERE }
| "="                 { EQUAL }
| ">"                 { GREATER }
| "*"                 { STAR }
| "FROM"              { FROM }
| ['a'-'z' 'A'-'Z' '_'](['a'-'z' 'A'-'Z' '0'-'9' '_'])* as ident { IDENT ident }
| ['0'-'9']+ as value { VALUE value }
| eof                 { EOF }
| _                   { failwith "Unexpected character" }
