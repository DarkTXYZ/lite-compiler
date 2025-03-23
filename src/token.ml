type token =
  | Keyword of string
  | TypeOrConstructorName of string
  | VariableName of string
  | Symbol of string
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | EOF
  | Error of string * Lexing.position

let string_of_token (t : token) : string =
  match t with
  | Keyword s -> "Keyword(" ^ s ^ ")"
  | TypeOrConstructorName s -> "Name(" ^ s ^ ")"
  | VariableName s -> "Var(" ^ s ^ ")"
  | Symbol s -> "Sym(" ^ s ^ ")"
  | BoolLit b -> "Bool(" ^ string_of_bool b ^ ")"
  | IntLit i -> "Int(" ^ string_of_int i ^ ")"
  | FloatLit f -> "Float(" ^ string_of_float f ^ ")"
  | CharLit c -> "Char('" ^ Char.escaped c ^ "')"
  | StringLit s -> "String(\"" ^ String.escaped s ^ "\")"
  | EOF -> "EOF"
  | Error (tok, pos) ->
      "Error(\"" ^ tok ^ "\", \"" ^ pos.pos_fname ^ "\", "
      ^ string_of_int pos.pos_lnum ^ ", " ^ string_of_int pos.pos_cnum ^ ")"
