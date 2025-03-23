type s_expr = Atom of string | List of s_expr list
type memoized = (string * int) list

type pretty_printed_s_expr =
  | Atom' of string * memoized
  | List' of pretty_printed_s_expr list * memoized

let ( >> ) f g x = g (f x)
let extract_mem e = match e with Atom' (_, mem) -> mem | List' (_, mem) -> mem

let rec init_pp s : pretty_printed_s_expr =
  match s with
  | Atom x -> Atom' (x, [ (x, String.length x) ])
  | List xs ->
      let xs' = List.map init_pp xs in
      let strs = List.map (extract_mem >> List.hd >> fst) xs' in
      let res = "(" ^ String.concat " " strs ^ ")" in
      List' (xs', [ (res, String.length res) ])

let rec repeat n c = if n <= 0 then "" else c ^ repeat (n - 1) c

let string_of_s_expr s =
  let rec find_fit mem space : (string * int) option =
    match mem with
    | [] -> None
    | ((_, sp) as res) :: rest ->
        if sp <= space then Some res else find_fit rest space
  in
  let rec format space em : (string * int) * pretty_printed_s_expr =
    match em with
    | Atom' (_, mem) -> (List.hd mem, em)
    | List' (xs, mem) -> (
        match find_fit mem space with
        | Some res -> (res, em)
        | None ->
            let xs' = List.map (format (space - 1)) xs in
            let res = List.map fst xs' in
            let strs = List.map fst res in
            let sp =
              1
              + List.fold_left
                  (fun x y -> if x >= y then x else y)
                  0 (List.map snd res)
            in
            let res =
              "("
              ^ String.concat ("\n" ^ repeat (80 - space + 1) " ") strs
              ^ "\n"
              ^ repeat (80 - space) " "
              ^ ")"
            in
            ((res, sp), List' (List.map snd xs', mem @ [ (res, sp) ])))
  in
  (fst >> fst) (format 80 (init_pp s))
