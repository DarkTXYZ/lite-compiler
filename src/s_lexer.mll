{
open S_parser

exception SyntaxError of string * Lexing.position
let error msg lexbuf =
  raise (SyntaxError(msg, Lexing.lexeme_start_p lexbuf))

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"

(** lexing rules **)
rule read = parse
  | line_terminator { Lexing.new_line lexbuf; read lexbuf }
  | [' ' '\t']+ { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | '\'' { read_char lexbuf }
  | '\"' { read_string lexbuf }
  | eof { EOF }
  | [^' ' '\t' '(' ')' '\'' '\"']+ as tok { ATOM tok }
and read_char = parse
  | '\'' {
      let res =
        ATOM ("'" ^ String.escaped (Buffer.contents string_buf) ^ "'") in
      Buffer.clear string_buf; res
    }
  | eof { error "unclosed character literal" lexbuf}
  | _ as c { Buffer.add_char string_buf c; read_char lexbuf }

and read_string = parse
  | '\"' {
      let res =
        ATOM ("\"" ^ String.escaped (Buffer.contents string_buf) ^ "\"") in
        Buffer.clear string_buf; res
    }
  | eof { error "unclosed string literal" lexbuf}
  | _ as c { Buffer.add_char string_buf c; read_string lexbuf }
