(** header section **)
%{
open S_expr
%}

(** declarations section **)

(* token declarations *)
%token LPAREN "("
%token RPAREN ")"
%token <string> ATOM
%token <char> CHARLIT
%token <string> STRINGLIT
%token EOF

(* types of nonterminals *)
%type <s_expr> start

(* start nonterminal *)
%start start
%%

(** rules section **)
start:
  | e = s_expr; EOF { e }

s_expr:
  | x = ATOM { Atom x }
  | c = CHARLIT { Atom ("'" ^ Char.escaped c ^ "'") }
  | s = STRINGLIT { Atom ("\"" ^ String.escaped s ^ "\"") }
  | "("; es = list(s_expr); ")" { List es }
