open Utils
open Token

let rec tokens (lexbuf : Lexing.lexbuf) : token list =
  match Lexer_a1.read lexbuf with
  | Error _ as e -> [ e ]
  | EOF -> [ EOF ]
  | tok -> tok :: tokens lexbuf

let lex (s : string) : token list =
  let lexbuf = Lexing.from_string s in
  tokens lexbuf

let pretty_lex (s : string) : string list = List.map string_of_token (lex s)

let lex_file (filename : string) : token list =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let _ = Lexing.set_filename lexbuf filename and toks = tokens lexbuf in
    close_in ic;
    toks
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e

let pretty_lex_file (s : string) : string list =
  List.map string_of_token (lex_file s)

let lex_test (s : string) : unit =
  let token_strings = pretty_lex_file s
  and filedir = Filename.dirname s
  and filename = Filename.basename s in
  let outdir = "out/01-lexing/" ^ filedir in
  create_directory outdir;
  let outfile = outdir ^ Filename.dir_sep ^ filename in
  let oc = open_out outfile in
  Printf.fprintf oc "%s\n" (String.concat "\n" token_strings);
  close_out oc;
  print_string ("output written to " ^ outfile);
  print_newline ()

let rec repl_loop () =
  print_string "enter file name: ";
  let filename = read_line () in
  if String.length filename == 0 then ()
  else (
    lex_test filename;
    repl_loop ())
