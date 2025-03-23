(** header: optional imports & setup **)
{
open Parser

exception SyntaxError of string * Lexing.position
let error msg lexbuf =
  raise (SyntaxError(msg, Lexing.lexeme_start_p lexbuf))

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+

let digit = ['0'-'9']
let int_lit = (digit)+
let float_lit = (digit)* '.' (digit)+

let character = [^'\n' '\r' '\"']
let alphabet_lowercase = ['a'-'z']
let alphabet_uppercase = ['A'-'Z']
let alphabet = alphabet_lowercase | alphabet_uppercase
let alphanumeric = alphabet | digit

let variable_alphabet = alphanumeric | '_' | '\''

let escape_character = '\\' ['\\' '\'' '\"' 'n' 'r' 't' 'b']
let string_lit = (character | escape_character)*
let type_or_constructor_name = alphabet_uppercase (variable_alphabet)*
let variable_name = alphabet_lowercase (variable_alphabet)*

let arithmetic_symbols = "+" | "-" | "*" | "/" | "//" | "%"
let comparison_symbols = "<" | "<=" | ">=" | ">" | "==" | "!="
let additional_symbols = "=" | "(" | ")" | "[" | "]" | ":" | "::" | "," | "->" | "=>" | "|" | "++" | "." | "$" | "_" | "\\" | "&&" | "||"
let symbols = arithmetic_symbols | comparison_symbols | additional_symbols

let keywords = "def" | "if" | "then" | "else" | "let" | "and" | "in" | "match" | "with" | "import" | "data" | "type"

(** lexing rules **)
rule read = parse
  (* Comments *)
  | '#' { skip_line lexbuf }
  | "(*" string_lit "*)" { skip_line lexbuf }

  (* Bool *)
  | "True" { BOOL_LIT(true) }
  | "False" { BOOL_LIT(false) }

  (* Keywords/Symbols/Type/Constructor/Variable *)
  | keywords as x { 
    match x with
      | "def" -> DEF
      | "if" -> IF
      | "then" -> THEN
      | "else" -> ELSE
      | "let" -> LET
      | "and" -> AND
      | "in" -> IN
      | "match" -> MATCH
      | "with" -> WITH
      | "import" -> IMPORT
      | "data" -> DATA
      | "type" -> TYPE
      | _ -> error (Lexing.lexeme lexbuf) lexbuf
  }
  | arithmetic_symbols as x { 
    match x with
      | "+" -> PLUS
      | "-" -> MINUS
      | "*" -> TIMES
      | "/" -> DIVIDE
      | "//" -> FDIVIDE
      | "%" -> MODULO
      | _ -> error (Lexing.lexeme lexbuf) lexbuf
  }
  | comparison_symbols as x {
    match x with
      | "<" -> LT
      | "<=" -> LE
      | ">" -> GT
      | ">=" -> GE
      | "==" -> EQ
      | "!=" -> NEQ
      | _ -> error (Lexing.lexeme lexbuf) lexbuf
  }
  | additional_symbols as x {
    match x with
      | "(" -> LPAREN
      | ")" -> RPAREN
      | "||" -> LOR
      | "&&" -> LAND
      | "=" -> ASSIGN
      | "[" -> LBRAC
      | "]" -> RBRAC
      | ":" -> COLON
      | "::" -> DCOLON
      | "," -> COMMA
      | "->" -> ARROW
      | "=>" -> DARROW
      | "|" -> BAR
      | "++" -> DPLUS
      | "." -> DOT
      | "$" -> DOLLAR
      | "_" -> UNDERSCORE
      | "\\" -> BSLASH
      | _ -> error (Lexing.lexeme lexbuf) lexbuf
  }
  | type_or_constructor_name as x { TYPE_OR_CONSTRUCTOR_NAME(x) }
  | variable_name as x { VARIABLE_NAME(x) }

  (* Integer *)
  | int_lit as x { INT_LIT(int_of_string x) }

  (* Float *)
  | float_lit as x { FLOAT_LIT(float_of_string x) }

  (* Character *)
  | '\'' (character as x) '\'' { CHAR_LIT(x) }
  | '\''  (escape_character as escape_str) '\'' { 
      match String.get escape_str 1 with
        | 'n' -> CHAR_LIT('\n') 
        | 'r' -> CHAR_LIT('\r') 
        | 't' -> CHAR_LIT('\t') 
        | 'b' -> CHAR_LIT('\b') 
        | '\'' -> CHAR_LIT('\'') 
        | '\"' -> CHAR_LIT('\"') 
        | '\\' -> CHAR_LIT('\\') 
        | _ -> error (Lexing.lexeme lexbuf) lexbuf
    }

  (* String *)
  | '\"' (string_lit as x) '\"' { STRING_LIT(x) }

  | whitespace { read lexbuf }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }

and skip_line =
  parse
    | line_terminator { Lexing.new_line lexbuf; read lexbuf }
    | eof { EOF }
    | _ { skip_line lexbuf }

(* CpecmuCompiler.A1_lexing.lex_test "src/main.cpe";; *)
