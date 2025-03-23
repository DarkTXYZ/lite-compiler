let string_of_position (pos : Lexing.position) : string =
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let string_of_positions (pos : Lexing.position * Lexing.position) : string =
  Printf.sprintf "%s:%d:%d-%d:%d" (fst pos).pos_fname (fst pos).pos_lnum
    ((fst pos).pos_cnum - (fst pos).pos_bol + 1)
    (snd pos).pos_lnum
    ((snd pos).pos_cnum - (snd pos).pos_bol + 1)

let create_directory (dir : string) : unit =
  let cwd = Sys.getcwd () and dirs = String.split_on_char '/' dir in
  let rec create_dir_rec dirs =
    match dirs with
    | [] -> Sys.chdir cwd
    | d :: ds ->
        if not (Sys.file_exists d) then Sys.mkdir d 0o755;
        Sys.chdir d;
        create_dir_rec ds
  in
  create_dir_rec dirs
