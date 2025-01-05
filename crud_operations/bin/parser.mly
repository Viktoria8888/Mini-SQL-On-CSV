%{
open Ast
%}

%token <string> IDENT
%token <string> VALUE
%token CREATE READ DELETE WHERE GREATER EQUAL
%token STAR COMMA FROM EOF

/* %nonassoc FROM  */
%start <Ast.command> main
%%

main:
| create_command EOF        { $1 }
| read_command EOF          { $1 }
| delete_command EOF        { $1 }

create_command:
| CREATE IDENT key_values   { Create ($2, $3) }

key_values:
| /* empty */               { [] }
| key_value key_values_tail { $1 :: $2 }

key_values_tail:
| /* empty */               { [] }
| key_value key_values_tail { $1 :: $2 }

key_value:
| IDENT EQUAL VALUE         { ($1, $3) }

read_command:
| READ IDENT FROM IDENT where_clause { Read ($2, $4, Some $5) }

where_clause:
| WHERE condition           { $1 }

condition:
| IDENT EQUAL VALUE         { EQUAL }
| IDENT GREATER VALUE       { GREATER }

delete_command:
| DELETE IDENT where_clause { Delete ($2, Some $3) }
| DELETE IDENT              { Delete ($2, None) }
