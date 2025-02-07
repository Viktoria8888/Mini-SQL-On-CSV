  %{
  open Ast
  %}

  %token <string> IDENT
  %token <string> VALUE 
  %token READ DELETE WHERE EQUAL GREATER
  %token STAR FROM EOF

  /* %nonassoc FROM  */
  %start <Ast.command> main
  %%

  main:
  | read_command EOF          { $1 }
  | delete_command EOF        { $1 }
  | select_command EOF        { $1 }

  read_command:
  | READ IDENT FROM IDENT { Read ($2, $4, None) }
  | READ IDENT FROM IDENT where_clause { Read ($2, $4, $5) }

  where_clause:
  | WHERE condition           { Some $2 }

  condition:
  | IDENT EQUAL VALUE         { Equal ($1, $3) }
  | IDENT GREATER VALUE       { Greater ($1, $3) }

  delete_command:
  | DELETE FROM; i = IDENT; clause=where_clause { Delete (i, clause) }

  select_command:
  | STAR FROM IDENT where_clause { SelectAll ($3, $4) }
  | STAR FROM IDENT              { SelectAll ($3, None) }