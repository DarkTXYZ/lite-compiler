type pos = Lexing.position * Lexing.position

type math_op =
  | Add
  | Sub
  | Mult
  | Div
  | FDiv (* Floor Division *)
  | Mod
  | Negate

type comp_op = Equal | NotEqual | Less | LessEQ | Greater | GreaterEQ
type logic_op = LAnd | LOr

type op =
  | MathOp of math_op
  | CompOp of comp_op
  | LogicOp of logic_op
  | Concat
  | StringConcat
  | Application
  | Compose
  | Pair

type literal =
  | IntLit of pos * int
  | FloatLit of pos * float
  | BoolLit of pos * bool
  | StringLit of pos * string
  | CharLit of pos * char
  | VarLit of pos * string
  | EmptyList of pos

type 't typeexpr =
  | PairType of {
      prop : 't;
      pos : pos;
      left : 't typeexpr;
      right : 't typeexpr;
    }
  | FuncType of { prop : 't; pos : pos; a : 't typeexpr; b : 't typeexpr }
  | ListType of { prop : 't; pos : pos; value : 't typeexpr }
  | PolymorphicType of {
      prop : 't;
      pos : pos;
      t : string;
      values : 't typeexpr list;
    }
  | Type of { prop : 't; pos : pos; value : string }
  | UnitType of { prop : 't; pos : pos }

type 't arg = Arg of pos * literal * 't typeexpr | UnitArg of pos

type pattern =
  | Wildcard of pos
  | EmptyList of pos
  | PairPattern of pos * pattern * pattern
  | ConcatPattern of pos * pattern * pattern
  | VarPattern of pos * string
  | LiteralPattern of pos * literal
  | ConstructorPattern of pos * string * pattern list

type 't expression =
  | UnaryExpr of { prop : 't; pos : pos; op : op; value : 't expression }
  | BinExpr of {
      prop : 't;
      pos : pos;
      op : op;
      left : 't expression;
      right : 't expression;
    }
  | IfExpr of {
      prop : 't;
      pos : pos;
      condition : 't expression;
      true_branch : 't expression;
      false_branch : 't expression;
    }
  | Literal of { prop : 't; pos : pos; value : literal }
  | LetExpr of {
      prop : 't;
      pos : pos;
      definitions : 't letbinding list;
      body : 't expression;
    }
    (* TODO *)
  | Lambda of {
      prop : 't;
      pos : pos;
      args : 't arg list;
      return_type : 't typeexpr;
      body : 't expression;
    }
    (* TODO *)
  | FuncAppWithParam of {
      prop : 't;
      pos : pos;
      func : 't expression;
      param : 't expression;
    }
    (* TODO *)
  | FuncApp of { prop : 't; pos : pos; f_name : string }
  | Match of {
      prop : 't;
      pos : pos;
      pattern : 't expression;
      cases : 't case list;
    }
(* TODO *)

and 't letbinding =
  (* TODO *)
  | LetBinding of pos * literal * 't expression
  | FuncLetBinding of pos * string * 't arg list * 't typeexpr * 't expression

and 't case = Case of pos * pattern * 't expression

type typedecl = TypeDecl of pos * string * string list
type 't constructor = Constructor of pos * string * 't typeexpr list

type 't defn =
  | DataDef of pos * typedecl * 't constructor list
  | TypeDef of pos * typedecl * 't typeexpr
  | FuncDef of pos * string * 't arg list * 't typeexpr * 't expression

type import_stm = Import of pos * string
type 't prog = Program of pos * import_stm list * 't defn list

let prop_of_typeexpr (expr : 't typeexpr) : 't =
  match expr with
  | PairType r -> r.prop
  | FuncType r -> r.prop
  | ListType r -> r.prop
  | PolymorphicType r -> r.prop
  | Type r -> r.prop
  | UnitType r -> r.prop

let prop_of_expr (expr : 't expression) : 't =
  match expr with
  | UnaryExpr r -> r.prop
  | Literal r -> r.prop
  | BinExpr r -> r.prop
  | IfExpr r -> r.prop
  | LetExpr r -> r.prop
  | FuncAppWithParam r -> r.prop
  | FuncApp r -> r.prop
  | Match r -> r.prop
  | Lambda r -> r.prop

let string_of_op op : string =
  match op with
  | MathOp x -> (
      match x with
      | Add -> "+"
      | Sub -> "-"
      | Negate -> "-"
      | Mult -> "*"
      | Div -> "/"
      | FDiv -> "//"
      | Mod -> "%")
  | Concat -> "::"
  | StringConcat -> "++"
  | Compose -> "."
  | Application -> "$"
  | Pair -> ","
  | CompOp x -> (
      match x with
      | Equal -> "=="
      | NotEqual -> "!="
      | Less -> "<"
      | LessEQ -> "<="
      | Greater -> ">"
      | GreaterEQ -> ">=")
  | LogicOp x -> ( match x with LAnd -> "&&" | LOr -> "||")

let rec replace_single_quote str =
  match str with
  | "" -> ""
  | s ->
      let first_char = String.get s 0 in
      let rest_of_string = String.sub s 1 (String.length s - 1) in
      if first_char = '\'' then "`" ^ replace_single_quote rest_of_string
      else String.make 1 first_char ^ replace_single_quote rest_of_string

let string_of_literal literal : string =
  match literal with
  | IntLit (_, i) -> string_of_int i
  | FloatLit (_, f) -> string_of_float f
  | BoolLit (_, b) -> string_of_bool b
  | StringLit (_, s) -> "\"" ^ String.escaped s ^ "\""
  | CharLit (_, c) -> "'" ^ Char.escaped c ^ "'"
  | VarLit (_, v) -> v
  | EmptyList _ -> "[]"

let rec string_of_type t : string =
  match t with
  | Type { value = t; _ } -> t
  | PairType { left = t1; right = t2; _ } ->
      "(, " ^ string_of_type t1 ^ " " ^ string_of_type t2 ^ ")"
  | ListType { value = t; _ } -> "([] " ^ string_of_type t ^ ")"
  | FuncType { a = t1; b = t2; _ } ->
      "(-> " ^ string_of_type t1 ^ " " ^ string_of_type t2 ^ ")"
  | UnitType _ -> "()"
  | PolymorphicType { t; values = tl; _ } ->
      "(" ^ t ^ " " ^ String.concat " " (List.map string_of_type tl) ^ ")"

let string_of_arg arg : string =
  match arg with
  | Arg (_, v, t) -> "(" ^ string_of_literal v ^ " " ^ string_of_type t ^ ")"
  | UnitArg _ -> "()"

let rec string_of_pattern pattern : string =
  match pattern with
  | Wildcard _ -> "_"
  | EmptyList _ -> "[]"
  | PairPattern (_, p1, p2) ->
      "(, " ^ string_of_pattern p1 ^ " " ^ string_of_pattern p2 ^ ")"
  | ConcatPattern (_, p1, p2) ->
      "(:: " ^ string_of_pattern p1 ^ " " ^ string_of_pattern p2 ^ ")"
  | VarPattern (_, v) -> v
  | LiteralPattern (_, l) -> string_of_literal l
  | ConstructorPattern (_, c, ts) -> (
      match ts with
      | [] -> c
      | _ ->
          "(" ^ c ^ "("
          ^ String.concat " " (List.map string_of_pattern ts)
          ^ ")" ^ ")")

let rec string_of_expr (expr : 't expression) : string =
  match expr with
  | UnaryExpr { op; value = e; _ } ->
      "(" ^ string_of_op op ^ " " ^ string_of_expr e ^ ")"
  | BinExpr { op; left = e1; right = e2; _ } ->
      "(" ^ string_of_op op ^ " " ^ string_of_expr e1 ^ " " ^ string_of_expr e2
      ^ ")"
  | IfExpr { condition = pred; true_branch = e1; false_branch = e2; _ } ->
      "(if " ^ string_of_expr pred ^ string_of_expr e1 ^ " " ^ string_of_expr e2
      ^ ")"
  | LetExpr { definitions = bindings; body = e; _ } ->
      "(let "
      ^ String.concat " " (List.map string_of_binding bindings)
      ^ string_of_expr e ^ ")"
  | Literal { value = l; _ } -> string_of_literal l
  | Lambda { args; return_type = t; body = e; _ } ->
      "(\\ " ^ "("
      ^ String.concat " " (List.map string_of_arg args)
      ^ ") " ^ string_of_type t ^ " " ^ string_of_expr e ^ ")"
  | FuncAppWithParam { func = f; param = p; _ } ->
      "(" ^ string_of_expr f ^ " " ^ string_of_expr p ^ ")"
  | FuncApp { f_name = f; _ } -> replace_single_quote f
  | Match { pattern = e; cases; _ } ->
      "(match " ^ string_of_expr e ^ "("
      ^ String.concat " " (List.map string_of_case cases)
      ^ "))"

and string_of_binding binding : string =
  match binding with
  | LetBinding (_, v, e) ->
      "((" ^ string_of_literal v ^ " " ^ string_of_expr e ^ "))"
  | FuncLetBinding (_, v, args, t, e) ->
      "((" ^ v ^ "("
      ^ String.concat " " (List.map string_of_arg args)
      ^ ")" ^ string_of_type t ^ string_of_expr e ^ "))"

and string_of_case case : string =
  match case with
  | Case (_, p, e) ->
      "(-> " ^ string_of_pattern p ^ " " ^ string_of_expr e ^ ")"

let string_of_typedecl = function
  | TypeDecl (_, t, []) -> t
  | TypeDecl (_, t, tl) -> "(" ^ t ^ " " ^ String.concat " " tl ^ ")"

let string_of_constructor = function
  | Constructor (_, c, []) -> c
  | Constructor (_, c, tl) ->
      "(" ^ c ^ " " ^ String.concat " " (List.map string_of_type tl) ^ ")"

let string_of_defn defn : string =
  match defn with
  | DataDef (_, td, cs) ->
      "(data " ^ string_of_typedecl td ^ "("
      ^ String.concat " " (List.map string_of_constructor cs)
      ^ ")" ^ ")"
  | TypeDef (_, td, te) ->
      "(type " ^ string_of_typedecl td ^ string_of_type te ^ ")"
  | FuncDef (_, v, args, ret, expr) ->
      "(def " ^ replace_single_quote v ^ "("
      ^ String.concat " " (List.map string_of_arg args)
      ^ ")" ^ string_of_type ret ^ " " ^ string_of_expr expr ^ ")"

let string_of_import import : string =
  match import with Import (_, v) -> "(import " ^ v ^ ")"

let string_of_prog prog : string =
  match prog with
  | Program (_, imports, defns) ->
      "(" ^ "("
      ^ String.concat " " (List.map string_of_import imports)
      ^ ")" ^ "("
      ^ String.concat " " (List.map string_of_defn defns)
      ^ ")" ^ ")"
