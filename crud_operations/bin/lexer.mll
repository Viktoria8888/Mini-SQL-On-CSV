{
open Parser
}

rule tokenize = parse
| "CREATE"            { CREATE }
| "READ"              { READ }
| "DELETE"            { DELETE }
| "WHERE"             { WHERE }
| "="                 { EQUAL }
| ">"                 { GREATER }
| "*"                 { STAR }
| ","                 { COMMA }
| "FROM"              { FROM }
| ['a'-'z' 'A'-'Z' '_']+ as ident { IDENT ident }
| ['0'-'9']+ as value { VALUE value }
| eof                 { EOF }
| _                   { failwith "Unexpected character" }
