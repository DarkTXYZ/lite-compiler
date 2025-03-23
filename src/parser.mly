(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> TYPE_OR_CONSTRUCTOR_NAME
%token <string> VARIABLE_NAME


%token PLUS "+" MINUS "-" TIMES "*" DIVIDE "/" FDIVIDE "//" MODULO "%"
%token EQ "==" NEQ "!=" GT ">" GE ">=" LT "<" LE "<=" 
%token LOR "||" LAND "&&"
%token LPAREN "(" RPAREN ")"
%token EOF
%token ASSIGN "=" LBRAC "[" RBRAC "]" COLON ":" DCOLON "::" COMMA ","
  ARROW "->" DARROW "=>" BAR "|" DPLUS "++" DOT "." DOLLAR "$" UNDERSCORE "_"
  BSLASH "\\"
%token DEF "def" IF "if" THEN "then" ELSE "else" LET "let" AND "and"
  IN "in" MATCH "match" WITH "with" IMPORT "import" DATA "data" TYPE "type"

(* precedence and associativity declarations *)
%nonassoc ELSE IN
%right ARROW DARROW

%right DOLLAR
%right LOR
%right LAND
%nonassoc EQ NEQ GT GE LT LE
%right DCOLON DPLUS
%left PLUS MINUS
%left TIMES DIVIDE FDIVIDE MODULO
%right DOT

(* starting nonterminal *)
%start <unit prog> start
%%

(** rules section **)
start:
  | prog = prog; EOF { prog }

prog:
  | import_stms = import_stms; def_stms = def_stms { Program($loc, import_stms, def_stms)} 
  | def_stms = def_stms { Program($loc, [], def_stms)} 

import_stms:
  | stm = import_stm; stms = import_stms  { stm::stms }
  | stm = import_stm { stm::[] }

import_stm:
  | IMPORT; l = VARIABLE_NAME; { Import($loc, l) }

def_stms:
  | d = def_stm; ds = def_stms { d::ds }
  | d = def_stm { d::[] }

def_stm:
  | d = defn; { d }

defn:
  | DEF; name = VARIABLE_NAME; args = arguments; COLON; ret = typeexpr; 
    ASSIGN; e = expr; { FuncDef($loc, name, args, ret, e) }
  | TYPE; td = typedecl; ASSIGN ; t = typeexpr { TypeDef($loc, td, t) }
  | DATA; td = typedecl; ASSIGN; c = constructors { DataDef($loc, td, c) }

constructors:
  | c1 = constructor; BAR; c2 = constructors { c1::c2 }
  | c = constructor { c::[] }

constructor:
  | t = TYPE_OR_CONSTRUCTOR_NAME { Constructor($loc, t, []) }
  | t = TYPE_OR_CONSTRUCTOR_NAME; ts = typevars { Constructor($loc, t, ts) }

typedecl:
  | t = TYPE_OR_CONSTRUCTOR_NAME { TypeDecl($loc, t, []) }
  | t = TYPE_OR_CONSTRUCTOR_NAME; ts = typedecl_items { TypeDecl($loc, t, ts) }

typedecl_items:
  | t = VARIABLE_NAME; ts = typedecl_items { t::ts }
  | t = VARIABLE_NAME { t::[] }

expr:
  | e = matchexpr { e }
  | e = simpleexpr { e }
  | LPAREN; e = expr; RPAREN; { e }

nested_expr:
  | e = nested_simpleexpr { e }
  | LPAREN; e = matchexpr; RPAREN { e }
  | LPAREN; e = nested_expr; RPAREN { e }

nested_simpleexpr:
  | f = funcapp(nested_expr) { f }
  | e = ifexpr(nested_expr) { e }
  | e = letexpr(nletbindings, nested_expr) { e }
  | e = lambdaexpr(nested_expr) { e }
  | e = binaryexpr(nested_expr) { e }
  | e = unaryexpr(nested_expr) { e }
  | l = literals { Literal { prop = (); pos = $loc; value = l } }

nletbindings:
  | a1 = nletbinding; AND; a2 = nletbindings { a1::a2 } 
  | a = nletbinding { a::[] } 

nletbinding:
  | v = VARIABLE_NAME; ASSIGN; e = nested_expr { LetBinding($loc, VarLit($loc, v), e) } 
  | f = VARIABLE_NAME; args = arguments; COLON
        ; ret = typeexpr; ASSIGN; e = nested_expr { FuncLetBinding($loc, f, args, ret, e) }

simpleexpr:
  | e = funcapp(expr) { e }
  | e = ifexpr(expr) { e }
  | e = letexpr(letbindings, expr) { e }
  | e = lambdaexpr(expr) { e }
  | e = binaryexpr(expr) { e }
  | e = unaryexpr(expr) { e }
  | l = literals { Literal{ prop = (); pos = $loc; value = l } }

funcapp(X):
  | f = func(X); p = param(X) { FuncAppWithParam { prop = (); pos = $loc; func = f; param = p } }

func(X): 
  | f = VARIABLE_NAME { FuncApp { prop = (); pos = $loc; f_name = f }}
  | f = funcapp(X) { f } 

param(X):
  | p = literals { Literal{ prop = (); pos = $loc; value = p } }
  | LPAREN; e = X; RPAREN { e }

ifexpr(X):
  | IF; e1 = X; THEN; e2 = X; ELSE; e3 = X 
    { IfExpr { prop = (); pos = $loc; condition = e1; true_branch = e2; false_branch = e3 }}

letexpr(D, X):
  | LET; a = D; IN; e = X { LetExpr { prop = (); pos = $loc; definitions = a; body = e }}

letbindings:
  | a1 = letbinding; AND; a2 = letbindings { a1::a2 } 
  | a = letbinding { a::[] } 

letbinding:
  | v = VARIABLE_NAME; ASSIGN; e = expr { LetBinding($loc, VarLit($loc, v), e) } 
  | f = VARIABLE_NAME; args = arguments; COLON
        ; ret = typeexpr; ASSIGN; e = expr { FuncLetBinding($loc, f, args, ret, e) }

matchexpr:
  | MATCH; e = expr; WITH; c = cases { Match { prop = (); pos = $loc; pattern = e; cases = c }}

cases:
  | c1 = case; c2 = cases { c1::c2 }
  | c = case { c::[] }

case:
  | BAR; p = pattern; ARROW; e = nested_expr { Case($loc, p, e) }

pattern:
  | UNDERSCORE { Wildcard($loc) }
  | t = constructor_pattern { t }
  | LPAREN; p1 = pattern; COMMA; p2 = pattern; RPAREN { PairPattern($loc, p1, p2) }
  | p1 = pattern; "::"; p2 = pattern { ConcatPattern($loc, p1, p2) }
  | l = literals { LiteralPattern($loc, l) }
  | LPAREN; p = pattern; RPAREN { p }

constructor_pattern:
  | t = TYPE_OR_CONSTRUCTOR_NAME { ConstructorPattern($loc, t, []) }
  | t = TYPE_OR_CONSTRUCTOR_NAME; ts = patterns { ConstructorPattern($loc, t, ts) }

patterns: 
  | p = nested_pattern; ps = patterns { p::ps }
  | p = nested_pattern { p::[] }

nested_pattern:
  | LPAREN; p = pattern; RPAREN { p }
  | l = literals { LiteralPattern($loc, l) }
  | LPAREN; p1 = pattern; COMMA; p2 = pattern; RPAREN { PairPattern($loc, p1, p2) }
  | UNDERSCORE { Wildcard($loc) }

lambdaexpr(X):
  | BSLASH; args = arguments; COLON; t = typeexpr; DARROW; e = X 
    { Lambda { prop = (); pos = $loc; args = args; return_type = t; body = e }}

arguments:
  | a = argument; ar = arguments { a::ar }
  | a = argument { a::[] }

argument:
  | LPAREN; v = VARIABLE_NAME; COLON; t = typeexpr; RPAREN { Arg($loc, VarLit($loc, v), t) }
  | LPAREN; RPAREN { UnitArg($loc) }

typeexpr:
  | t = TYPE_OR_CONSTRUCTOR_NAME { Type { prop = (); pos = $loc; value = t } }
  | t = TYPE_OR_CONSTRUCTOR_NAME; ts = typevars 
    { PolymorphicType { prop = (); pos = $loc; t = t; values = ts } }
  | t = VARIABLE_NAME { Type { prop = (); pos = $loc; value = t } }
  | LBRAC; t = typeexpr; RBRAC; { ListType { prop = (); pos = $loc; value = t } } 
  | LPAREN; t1 = typeexpr; COMMA; t2 = typeexpr; RPAREN { PairType { prop = (); pos = $loc; left = t1; right = t2 } }
  | t1 = typeexpr; ARROW; t2 = typeexpr { FuncType { prop = (); pos = $loc; a = t1; b = t2 } }
  | LPAREN; t = typeexpr; RPAREN { t }
  | LPAREN; RPAREN { UnitType { prop = (); pos = $loc } }

typevars:
  | t = typevar; ts = typevars { t::ts }
  | t = typevar { t::[] }

typevar:
  | LPAREN; t = TYPE_OR_CONSTRUCTOR_NAME; ts = typevars; RPAREN 
    { PolymorphicType { prop = (); pos = $loc; t = t; values = ts } }
  | t = TYPE_OR_CONSTRUCTOR_NAME { Type { prop = (); pos = $loc; value = t } }
  | t = VARIABLE_NAME { Type { prop = (); pos = $loc; value = t } }

unaryexpr(X):
  | "-"; e = X { UnaryExpr { prop= (); pos = $loc; op = MathOp(Negate); value = e } }

binaryexpr(X):
  | e1 = X; "+"; e2 = X { BinExpr { prop = (); pos= $loc; op= MathOp (Add); left= e1; right= e2} }
  | e1 = X; "-"; e2 = X { BinExpr { prop = (); pos= $loc; op= MathOp (Sub); left= e1; right= e2} }
  | e1 = X; "*"; e2 = X { BinExpr { prop = (); pos= $loc; op= MathOp (Mult); left= e1; right= e2} }
  | e1 = X; "/"; e2 = X { BinExpr { prop = (); pos= $loc; op= MathOp(Div); left= e1; right= e2} }
  | e1 = X; "//"; e2 = X { BinExpr { prop = (); pos= $loc; op= MathOp (FDiv); left= e1; right= e2} }
  | e1 = X; "%"; e2 = X { BinExpr { prop = (); pos= $loc; op= MathOp (Mod); left= e1; right= e2} }
  | e1 = X; "=="; e2 = X { BinExpr{ prop = (); pos= $loc; op= CompOp (Equal); left= e1; right= e2} }
  | e1 = X; "!="; e2 = X { BinExpr{ prop = (); pos= $loc; op= CompOp (NotEqual); left= e1; right= e2} }
  | e1 = X; "<"; e2 = X { BinExpr{ prop = (); pos= $loc; op= CompOp (Less); left= e1; right= e2} }
  | e1 = X; "<="; e2 = X { BinExpr{ prop = (); pos= $loc; op= CompOp (LessEQ); left= e1; right= e2} }
  | e1 = X; ">="; e2 = X { BinExpr{ prop = (); pos= $loc; op= CompOp (GreaterEQ); left= e1; right= e2} }
  | e1 = X; ">"; e2 = X { BinExpr{ prop = (); pos= $loc; op= CompOp (Greater); left= e1; right= e2} }
  | e1 = X; "||"; e2 = X { BinExpr{ prop = (); pos= $loc; op= LogicOp (LOr); left= e1; right= e2} }
  | e1 = X; "&&"; e2 = X { BinExpr{ prop = (); pos= $loc; op= LogicOp (LAnd); left= e1; right= e2} }
  | e1 = X; "::"; e2 = X { BinExpr{ prop = (); pos= $loc; op= Concat; left= e1; right= e2} }
  | e1 = X; "."; e2 = X { BinExpr{ prop = (); pos= $loc; op= Compose; left= e1; right= e2} }
  | e1 = X; "$"; e2 = X { BinExpr{ prop = (); pos= $loc; op= Application; left= e1; right= e2} }
  | e1 = X; "++"; e2 = X { BinExpr{ prop = (); pos= $loc; op= StringConcat; left= e1; right= e2} }
  | LPAREN; e1 = X; ","; e2 = X; RPAREN { BinExpr{ prop = (); pos= $loc; op= Pair; left= e1; right=e2} }

literals:
  | l = CHAR_LIT { CharLit($loc, l) }
  | l = STRING_LIT { StringLit($loc, l) }
  | l = INT_LIT { IntLit ($loc, l) }
  | l = FLOAT_LIT { FloatLit ($loc, l)}
  | l = BOOL_LIT { BoolLit($loc, l) }
  | v = VARIABLE_NAME { VarLit($loc, v) }
  | LBRAC; RBRAC { EmptyList($loc) }

(* CpecmuCompiler.A2_parsing.parse_test "src/main.cpe";; *)
(* menhir --strict --dump --explain src/parser.mly *)
