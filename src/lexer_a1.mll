(** header: optional imports & setup **)
{
open Token

let error msg lexbuf =
  Error(msg, Lexing.lexeme_start_p lexbuf)

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let digit = ['0'-'9']
let digit = ['0'-'9']

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | "+" { Symbol("+") }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
