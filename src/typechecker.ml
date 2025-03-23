open Utils
open Ast

(* TODO: define your own types *)
type lang_type =
  | Int
  | Double
  | Bool
  | Char
  | String
  | Pair of lang_type * lang_type
  | List of lang_type
  | EmptyListType
  | Arrow of lang_type list
  | Generic of string
  | Unit

module Env = struct
  type tbl = (string, lang_type) Hashtbl.t

  let default () : tbl = Hashtbl.create 100

  let lookup tbl (key : string) : lang_type option =
    try Some (Hashtbl.find tbl key) with Not_found -> None

  let add tbl (key : string) (value : lang_type) : unit =
    Hashtbl.add tbl key value

  let replace tbl (key : string) (value : lang_type) : unit =
    Hashtbl.replace tbl key value

  let copy tbl : tbl =
    let new_tbl = Hashtbl.create (Hashtbl.length tbl) in
    Hashtbl.iter (fun key value -> Hashtbl.add new_tbl key value) tbl;
    new_tbl
end

let rec string_of_type (t : lang_type) : string =
  match t with
  | Int -> "Integer"
  | Bool -> "Bool"
  | Double -> "Double"
  | Char -> "Char"
  | String -> "String"
  | Pair (t1, t2) -> "(" ^ string_of_type t1 ^ ", " ^ string_of_type t2 ^ ")"
  | EmptyListType -> "[]"
  | List tl -> "[" ^ string_of_type tl ^ "]"
  | Unit -> "()"
  | Arrow args ->
      "(" ^ String.concat " -> " (List.map string_of_type args) ^ ")"
  | Generic name -> name

(* need to have some env to check for generic type, if it is defined or not *)
let rec typecheck_typeexpr (env : Env.tbl) (t : unit typeexpr) :
    lang_type typeexpr option =
  match t with
  | Type ({ value = "Integer"; _ } as r) -> Some (Type { r with prop = Int })
  | Type ({ value = "Bool"; _ } as r) -> Some (Type { r with prop = Bool })
  | Type ({ value = "Double"; _ } as r) -> Some (Type { r with prop = Double })
  | Type ({ value = "Char"; _ } as r) -> Some (Type { r with prop = Char })
  | Type ({ value = "String"; _ } as r) -> Some (Type { r with prop = String })
  | Type r ->
      Some (Type { r with prop = Generic r.value })
      (* need to have check here *)
  | PairType r -> (
      match (typecheck_typeexpr env r.left, typecheck_typeexpr env r.right) with
      | Some left', Some right' ->
          let t1 = prop_of_typeexpr left' and t2 = prop_of_typeexpr right' in
          Some
            (PairType
               { r with prop = Pair (t1, t2); left = left'; right = right' })
      | _ -> None)
  | ListType r -> (
      match typecheck_typeexpr env r.value with
      | Some value' ->
          let t = prop_of_typeexpr value' in
          Some (ListType { r with prop = List t; value = value' })
      | _ -> None)
  | FuncType r -> (
      match (typecheck_typeexpr env r.a, typecheck_typeexpr env r.b) with
      | Some a', Some b' ->
          Some
            (FuncType
               {
                 r with
                 prop = Arrow [ prop_of_typeexpr a'; prop_of_typeexpr b' ];
                 a = a';
                 b = b';
               })
      | _ -> None)
  (* TODO, I think this is not correct right now *)
  | PolymorphicType r -> (
      match
        List.fold_left
          (fun acc x ->
            match acc with
            | None -> None
            | Some acc' -> (
                match typecheck_typeexpr env x with
                | Some x' -> Some (x' :: acc')
                | _ -> None))
          (Some []) r.values
      with
      | Some values' ->
          Some
            (PolymorphicType
               {
                 r with
                 prop = Arrow (List.map prop_of_typeexpr values');
                 values = values';
               })
      | _ -> None)
  | UnitType r -> Some (UnitType { r with prop = Unit })

let rec is_same_type (t1 : lang_type) (t2 : lang_type) : bool =
  match (t1, t2) with
  | Int, Int -> true
  | Double, Double -> true
  | Bool, Bool -> true
  | Char, Char -> true
  | String, String -> true
  | Unit, Unit -> true
  | Pair (t1, t2), Pair (t3, t4) -> is_same_type t1 t3 && is_same_type t2 t4
  | List t1, List t2 -> is_same_type t1 t2
  | Arrow l1, Arrow l2 -> List.equal (fun a b -> is_same_type a b) l1 l2
  | Arrow (Unit::t1::[]), t2 -> is_same_type t1 t2
  | t1, Arrow (Unit::t2::[]) -> is_same_type t1 t2
  | _ -> false

let typecheck_pair (t1 : lang_type) (t2 : lang_type) (t3 : lang_type)
    (t4 : lang_type) : bool =
  is_same_type t1 t3 && is_same_type t2 t4

(** Special name for tracking var that has been in env already.

    Be super careful which `pos` to use.
    *)
let s_name_alrin (pos : pos) (name : string) : string =
  "__" ^ name ^ ":" ^ string_of_positions pos ^ "__"

let typecheck_arg (parent_pos : pos)
    (acc : (Env.tbl * lang_type arg list) option) (arg : unit arg) :
    (Env.tbl * lang_type arg list) option =
  match acc with
  | None -> None
  | Some (env, l) -> (
      match arg with
      | Arg (pos, literal, typeexpr) -> (
          match (literal, typecheck_typeexpr env typeexpr) with
          | VarLit (var_pos, name), Some t -> (
              match Env.lookup env (s_name_alrin parent_pos name) with
              | Some _ ->
                  prerr_string
                    (string_of_positions var_pos
                    ^ ": argument " ^ name ^ " is duplicated in the lambda \n");
                  None
              | None ->
                  Env.add env
                    (s_name_alrin parent_pos name)
                    (prop_of_typeexpr t);
                  Env.replace env name (prop_of_typeexpr t);
                  Some (env, Arg (pos, literal, t) :: l))
          | _ ->
              prerr_string
                (string_of_positions pos ^ ": expected an argument \n");
              None)
      | UnitArg _ as arg -> Some (env, arg :: l))

(** Don't forget to copy the env, or we should create new one in this? *)
let typecheck_args (parent_pos : pos) (env : Env.tbl) (args : unit arg list) :
    (Env.tbl * lang_type arg list) option =
  match args with
  | [] ->
      prerr_string (string_of_positions parent_pos ^ ": emtpy lambda ?? \n");
      None
  | xs -> List.fold_left (typecheck_arg parent_pos) (Some (env, [])) xs

let args_to_langtypes (args : lang_type arg list) : lang_type list =
  List.map
    (fun arg ->
      match arg with UnitArg _ -> Unit | Arg (_, _, t) -> prop_of_typeexpr t)
    args

let curry_ver (args : lang_type list) (ret : lang_type) : lang_type =
  List.fold_right (fun x acc -> Arrow [ x; acc ]) args ret

let typecheck_letbinding
    (typecheck_expr : Env.tbl -> unit expression -> lang_type expression option)
    (parent_pos : pos) (acc : (Env.tbl * lang_type letbinding list) option)
    (item : unit letbinding) : (Env.tbl * lang_type letbinding list) option =
  match acc with
  | None -> None
  | Some (env, bindings) -> (
      match item with
      (* LET-VAR *)
      | LetBinding (pos, literal, value) -> (
          match (literal, typecheck_expr env value) with
          | VarLit (var_pos, name), Some expr -> (
              match Env.lookup env (s_name_alrin parent_pos name) with
              | Some _ ->
                  prerr_string
                    (string_of_positions var_pos
                    ^ ": variable " ^ name ^ " is duplicated in let bindings \n"
                    );
                  None
              | None ->
                  Env.add env (s_name_alrin parent_pos name) (prop_of_expr expr);
                  Env.replace env name (prop_of_expr expr);
                  Some (env, LetBinding (pos, literal, expr) :: bindings))
          | _ ->
              prerr_string (string_of_positions pos ^ ": expected a variable \n");
              None)
      (* LET_FUNC *)
      | FuncLetBinding (pos, name, args, ret_type, body) -> (
          match typecheck_args parent_pos env args with
          | Some (new_env, args') -> (
              match
                ( typecheck_typeexpr new_env ret_type,
                  typecheck_expr new_env body )
              with
              | Some rt, Some body' ->
                  let t1, t2 = (prop_of_typeexpr rt, prop_of_expr body') in
                  if is_same_type t1 t2 then (
                    match Env.lookup env (s_name_alrin parent_pos name) with
                    | Some _ ->
                        prerr_string
                          (string_of_positions pos
                         ^ ": duplicated function binding \"" ^ name ^ "\" \n");
                        None
                    | _ ->
                        let f_type =
                          curry_ver (args_to_langtypes args')
                            (prop_of_typeexpr rt)
                        in
                        Env.add env (s_name_alrin parent_pos name) f_type;
                        Env.replace env name f_type;
                        Some
                          ( env,
                            FuncLetBinding (pos, name, args', rt, body')
                            :: bindings ))
                  else (
                    prerr_string
                      (string_of_positions pos
                     ^ ": expected the body of function to return the same \
                        type, expect " ^ string_of_type t1 ^ " but got "
                     ^ string_of_type t2 ^ "\n");
                    None)
              | _ -> None)
          | _ -> None))

let typecheck_letbindings (parent_pos : pos) (env : Env.tbl)
    (ls : unit letbinding list)
    (typecheck_expr : Env.tbl -> unit expression -> lang_type expression option)
    : (Env.tbl * lang_type letbinding list) option =
  match ls with
  | [] ->
      prerr_string (string_of_positions parent_pos ^ ": emtpy letbindings ?? \n");
      None
  | xs ->
      List.fold_left
        (typecheck_letbinding typecheck_expr parent_pos)
        (Some (env, []))
        xs

let rec pattern_matching (env : Env.tbl) (l : lang_type) (pos : pos)
    (p : pattern) : Env.tbl option =
  print_string ("Lang type: " ^ string_of_type l ^ "\n");
  print_string ("Pattern: " ^ string_of_pattern p ^ "\n");
  match p with
  | Wildcard _ -> Some env
  | EmptyList _ -> (
      print_string "YEAH1";
      match l with
      | List _ ->
          print_string "YEAH2";
          Some env
      | _ -> None)
  | LiteralPattern (_, literal) -> (
      match (literal, l) with
      | IntLit (_, _), Int
      | FloatLit (_, _), Double
      | BoolLit (_, _), Bool
      | CharLit (_, _), Char ->
          Some env
      | VarLit (_, v), _ -> (
          match Env.lookup env (s_name_alrin pos v) with
          | Some _ ->
              prerr_string
                (string_of_positions pos ^ ": Variable " ^ v
               ^ " is bound several times in this matching" ^ " \n");
              None
          | None ->
              Env.add env (s_name_alrin pos v) l;
              Env.replace env v l;
              Some env)
      | EmptyList _, List _ -> Some env
      | _ -> None)
  | PairPattern (_, p1, p2) -> (
      (* print_string (string_of_pattern p1 ^ "," ^ string_of_pattern p2 ^ "\n"); *)
      match l with
      | Pair (ll, lr) -> (
          match pattern_matching env ll pos p1 with
          | Some env' -> pattern_matching env' lr pos p2
          | None -> None)
      | _ -> None)
  | ConcatPattern (_, p1, p2) -> (
      match l with
      | List t -> (
          let env' = pattern_matching env t pos p1 in
          match env' with
          | Some e ->
              let env'' = pattern_matching e (List t) pos p2 in
              env''
          | _ -> None)
      | _ -> None)
  | _ ->
      prerr_string "Not implemented yet";
      None
(* Constructor Pattern *)
(* Environment Scoping *)

let typecheck_case
    (typecheck_expr : Env.tbl -> unit expression -> lang_type expression option)
    (pattern_type : lang_type) (env : Env.tbl)
    (acc : (lang_type option * lang_type case list) option) (case : unit case) =
  match acc with
  | None -> None
  | Some (ret_type, cases') -> (
      match ret_type with
      | None -> (
          print_string "Ret type: None\n";
          print_string ("Current Case: " ^ string_of_case case ^ "\n");
          print_string
            ("Checked Cases: "
            ^ String.concat " " (List.map string_of_case cases')
            ^ "\n");
          match case with
          | Case (pos, p, e) -> (
              match pattern_matching env pattern_type pos p with
              | Some env' -> (
                  match typecheck_expr env' e with
                  | Some e' -> (
                      match prop_of_expr e' with
                      | t'' -> Some (Some t'', Case (pos, p, e') :: cases'))
                  | _ -> None)
              | _ -> None))
      | Some t' -> (
          print_string ("Ret type: " ^ string_of_type t' ^ "\n");
          print_string ("Current Case: " ^ string_of_case case ^ "\n");
          print_string
            ("Checked Cases: "
            ^ String.concat " " (List.map string_of_case cases')
            ^ "\n");
          match case with
          | Case (pos, p, e) -> (
              match pattern_matching env pattern_type pos p with
              | Some env' -> (
                  match typecheck_expr env' e with
                  | Some e' -> (
                      match prop_of_expr e' with
                      | t'' ->
                          print_string
                            ("Expected type: " ^ string_of_type t' ^ "\n");
                          print_string
                            ("Outcome type: " ^ string_of_type t'' ^ "\n");
                          if is_same_type t' t'' then
                            Some (Some t', Case (pos, p, e') :: cases')
                          else (
                            prerr_string
                              (string_of_positions pos
                             ^ ": return expression has type "
                             ^ string_of_type t''
                             ^ " but an expression was expected of type "
                             ^ string_of_type t' ^ "\n");
                            None))
                  | _ -> None)
              | _ -> None)))

let rec typecheck_expr (env : Env.tbl) (expr : unit expression) :
    lang_type expression option =
  match expr with
  | Literal r -> (
      match r.value with
      | IntLit _ -> Some (Literal { r with prop = Int })
      | FloatLit _ -> Some (Literal { r with prop = Double })
      | BoolLit _ -> Some (Literal { r with prop = Bool })
      | CharLit _ -> Some (Literal { r with prop = Char })
      | StringLit _ -> Some (Literal { r with prop = String })
      | EmptyList _ -> Some (Literal { r with prop = EmptyListType })
      | VarLit (pos, name) -> (
          (* VAR *)
          match Env.lookup env name with
          | Some t -> Some (Literal { r with prop = t })
          | None ->
              prerr_string
                (string_of_positions pos ^ ": variable " ^ name
               ^ " is not defined yet\n");
              None))
  | BinExpr ({ op = Pair; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          | t1, t2 ->
              Some
                (BinExpr
                   { r with prop = Pair (t1, t2); left = left'; right = right' })
          )
      | _ -> None)
  | BinExpr ({ op = MathOp Add; _ } as r)
  | BinExpr ({ op = MathOp Sub; _ } as r)
  | BinExpr ({ op = MathOp Mult; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* ARITH-INT *)
          | Int, Int ->
              Some (BinExpr { r with prop = Int; left = left'; right = right' })
          (* ARITH-DOUBLE *)
          | Double, Double ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          (* ARITH-COERCE-L *)
          | Int, Double ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          (* ARITH-COERCE-R *)
          | Double, Int ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          | t1, t2 ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected TODO operands, but found " ^ string_of_type t1
               ^ ", " ^ string_of_type t2 ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = MathOp Mod; _ } as r) -> (
      (* ARITH-INT *)
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          | Int, Int ->
              Some (BinExpr { r with prop = Int; left = left'; right = right' })
          (* ARITH-COERCE-R *)
          | Double, Int ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          | t1, t2 ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected TODO operands, but found " ^ string_of_type t1
               ^ ", " ^ string_of_type t2 ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = MathOp Div; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* ARITH-INT-DIV *)
          | Int, Int ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          (* ARITH-DOUBLE *)
          | Double, Double ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          (* ARITH-COERCE-L *)
          | Int, Double ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          (* ARITH-COERCE-R *)
          | Double, Int ->
              Some
                (BinExpr { r with prop = Double; left = left'; right = right' })
          | t1, t2 ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected TODO operands, but found " ^ string_of_type t1
               ^ ", " ^ string_of_type t2 ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = MathOp FDiv; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* ARITH-INT *)
          | Int, Int ->
              Some (BinExpr { r with prop = Int; left = left'; right = right' })
          (* ARITH-DOUBLE-DIV *)
          | Double, Int ->
              Some (BinExpr { r with prop = Int; left = left'; right = right' })
          | t1, t2 ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected TODO operands, but found " ^ string_of_type t1
               ^ ", " ^ string_of_type t2 ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = CompOp Equal; _ } as r)
  | BinExpr ({ op = CompOp NotEqual; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* NUMERIC-COMP *)
          | Int, Int | Int, Double | Double, Int | Double, Double ->
              Some
                (BinExpr { r with prop = Bool; left = left'; right = right' })
          (* COMP *)
          | _ as x ->
              let t1 = fst x and t2 = snd x in
              if is_same_type t1 t2 then
                Some
                  (BinExpr { r with prop = Bool; left = left'; right = right' })
              else (
                prerr_string
                  (string_of_positions r.pos ^ ": expected the result to be \n"
                 ^ "- Int, Int\n" ^ "- Int, Double\n" ^ "- Double, Int\n"
                 ^ "- Double, Double\n" ^ "- X, X when X is any literal types\n"
                 ^ ", but found " ^ string_of_type t1 ^ ", " ^ string_of_type t2
                 ^ "\n");
                None))
      | _ -> None)
  | BinExpr ({ op = CompOp Less; _ } as r)
  | BinExpr ({ op = CompOp LessEQ; _ } as r)
  | BinExpr ({ op = CompOp Greater; _ } as r)
  | BinExpr ({ op = CompOp GreaterEQ; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* NUMERIC-COMP *)
          | Int, Int | Int, Double | Double, Int | Double, Double ->
              Some
                (BinExpr { r with prop = Bool; left = left'; right = right' })
          | _ as x ->
              prerr_string
                (string_of_positions r.pos ^ ": expected the result to be \n"
               ^ "- Int, Int\n" ^ "- Int, Double\n" ^ "- Double, Int\n"
               ^ "- Double, Double\n" ^ ", but found "
                ^ string_of_type (fst x)
                ^ ", "
                ^ string_of_type (snd x)
                ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = LogicOp LAnd; _ } as r)
  | BinExpr ({ op = LogicOp LOr; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* BOOL *)
          | Bool, Bool ->
              Some
                (BinExpr { r with prop = Bool; left = left'; right = right' })
          | _ as x ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected the result to be Bool, Bool ,but found "
                ^ string_of_type (fst x)
                ^ ", "
                ^ string_of_type (snd x)
                ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = Concat; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* LIST-CONS with empty list *)
          | t, EmptyListType ->
              Some
                (BinExpr { r with prop = List t; left = left'; right = right' })
          (* LIST-CONS *)
          | t1, List t2 ->
              if is_same_type t1 t2 then
                Some
                  (BinExpr
                     { r with prop = List t1; left = left'; right = right' })
              else (
                prerr_string
                  (string_of_positions r.pos
                 ^ ": expected the result to be X::[X] when X is any literal \
                    types ,but found " ^ string_of_type t1 ^ "::"
                 ^ string_of_type (List t2) ^ "\n");
                None)
          | _ as t ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected the result to be TODO ,but found "
                ^ string_of_type (fst t)
                ^ "::"
                ^ string_of_type (snd t)
                ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = StringConcat; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          (* LIST-CONCAT *)
          | List t1, List t2 ->
              if is_same_type t1 t2 then
                Some
                  (BinExpr
                     { r with prop = List t1; left = left'; right = right' })
              else (
                prerr_string
                  (string_of_positions r.pos
                 ^ ": expected the result to be [X]++[X] when X is any literal \
                    types ,but found " ^ string_of_type (List t1) ^ "++"
                 ^ string_of_type (List t2) ^ "\n");
                None)
          | _ as t ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected the result to be [X]++[X] when X is any literal \
                  types ,but found "
                ^ string_of_type (fst t)
                ^ "++"
                ^ string_of_type (snd t)
                ^ "\n");
              None)
      | _ -> None)
  | BinExpr ({ op = Compose; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some left', Some right' -> (
          match (prop_of_expr left', prop_of_expr right') with
          | Arrow l1, Arrow l2 -> (
              match (l1, l2) with
              | beta' :: xs, alpha :: ys ->
                  let beta = List.hd (List.rev ys)
                  and gamma = List.hd (List.rev xs) in
                  if is_same_type beta beta' then
                    Some
                      (BinExpr
                         {
                           r with
                           prop = Arrow [ alpha; gamma ];
                           left = left';
                           right = right';
                         })
                  else (
                    prerr_string
                      (string_of_positions r.pos
                     ^ ": expected the return of right <" ^ string_of_type beta
                     ^ "> and first argument of the left <"
                     ^ string_of_type beta' ^ "> to be the same type\n");
                    None)
              | _ ->
                  prerr_string
                    (string_of_positions r.pos
                   ^ ": incomplete function type (internal error) \n");
                  None)
          | _ ->
              prerr_string
                (string_of_positions r.pos
               ^ ": can only compose function with function\n");
              None)
      | _ -> None)
  | UnaryExpr ({ op = MathOp Negate; _ } as r) -> (
      (* ARITH-NEG *)
      match typecheck_expr env r.value with
      | Some value' -> (
          match prop_of_expr value' with
          | Int -> Some (UnaryExpr { r with prop = Int; value = value' })
          | Double -> Some (UnaryExpr { r with prop = Int; value = value' })
          | t ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected int or double operand, but found "
               ^ string_of_type t ^ "\n");
              None)
      | _ -> None)
  | IfExpr r -> (
      match
        ( typecheck_expr env r.condition,
          typecheck_expr env r.true_branch,
          typecheck_expr env r.false_branch )
      with
      | Some condition', Some true_b, Some false_b -> (
          match
            (prop_of_expr condition', prop_of_expr true_b, prop_of_expr false_b)
          with
          | Bool, t1, t2 ->
              if is_same_type t1 t2 then
                Some
                  (IfExpr
                     {
                       r with
                       prop = prop_of_expr true_b;
                       condition = condition';
                       true_branch = true_b;
                       false_branch = false_b;
                     })
              else (
                prerr_string
                  (string_of_positions r.pos
                 ^ ": expected the result to be the same type, but found "
                 ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ "\n");
                None)
          | _ ->
              prerr_string
                (string_of_positions r.pos
               ^ ": expected the condition to be bool \n");
              None)
      | _ -> None)
  | LetExpr r -> (
      match
        typecheck_letbindings r.pos (Env.copy env) r.definitions typecheck_expr
      with
      | Some (new_env, letbindings) -> (
          match typecheck_expr new_env r.body with
          | Some body' ->
              Some
                (LetExpr
                   {
                     r with
                     prop = prop_of_expr body';
                     body = body';
                     definitions = letbindings;
                   })
          | _ ->
              prerr_string
                (string_of_positions r.pos
               ^ ": somethings wrong with body of this letbindings\n");
              None)
      | _ -> None)
  | Lambda r -> (
      (* LAMBDA-DECL *)
      match
        ( typecheck_args r.pos (Env.copy env) r.args,
          typecheck_typeexpr env r.return_type )
      with
      | Some (new_env, args'), Some rt -> (
          match typecheck_expr new_env r.body with
          | Some body' ->
              let t1, t2 = (prop_of_typeexpr rt, prop_of_expr body') in
              if is_same_type t1 t2 then
                Some
                  (Lambda
                     {
                       r with
                       prop = curry_ver (args_to_langtypes args') t1;
                       return_type = rt;
                       body = body';
                       args = args';
                     })
              else (
                prerr_string
                  (string_of_positions r.pos
                 ^ ": expected the body of lambda to return the same type, \
                    expect " ^ string_of_type t1 ^ " but got "
                 ^ string_of_type t2 ^ "\n");
                None)
          | _ -> None)
      | _ -> None)
  | Match r -> (
      match typecheck_expr (Env.copy env) r.pattern with
      | Some pattern_type -> (
          match
            List.fold_left
              (typecheck_case typecheck_expr
                 (prop_of_expr pattern_type)
                 (Env.copy env))
              (Some (None, []))
              r.cases
          with
          | Some (Some lang_type, cases') ->
              Some
                (Match
                   {
                     r with
                     prop = lang_type;
                     pattern = pattern_type;
                     cases = cases';
                   })
          | _ ->
              prerr_string
                (string_of_positions r.pos
               ^ ": somethings wrong with [cases] in [match exp with cases]\n");
              None)
      | _ ->
          prerr_string
            (string_of_positions r.pos
           ^ ": somethings wrong with [exp] in [match exp with cases] \n");
          None)
  | FuncApp r -> (
      match Env.lookup env r.f_name with
      | Some lang_type ->
          prerr_string
            (string_of_positions r.pos ^ "|FuncApp create " ^ r.f_name ^ " \n");

          Some (FuncApp { r with prop = lang_type; f_name = r.f_name })
      | _ ->
          prerr_string
            (string_of_positions r.pos ^ "| not found in lookup table \n");
          None)
  | FuncAppWithParam r -> (
      match (typecheck_expr env r.func, typecheck_expr env r.param) with
      | Some func', Some param' -> (
          print_string (string_of_type (prop_of_expr func') ^ ":a \n");
          print_string (string_of_type (prop_of_expr param') ^ ":b \n");
          match (prop_of_expr func', prop_of_expr param') with
          | Arrow f, p ->
              if is_same_type (List.hd f) p then (
                match f with
                | x :: [] ->
                    (* FUNC-APP-L *)
                    print_string
                      (string_of_positions r.pos
                     ^ "|FuncAppWithParam x::[] create \n");
                    Some
                      (FuncAppWithParam
                         { r with prop = x; func = func'; param = param' })
                | _ :: xs ->
                    print_string
                      (string_of_positions r.pos
                     ^ "|FuncAppWithParam x::xs  create \n");
                    Some
                      (FuncAppWithParam
                         {
                           r with
                           prop = List.hd xs;
                           (* prop = Arrow xs; *)
                           (* prop =   typecheck_expr env xs; *)
                           func = func';
                           param = param';
                         })
                | _ ->
                    prerr_string
                      (string_of_positions r.pos ^ "| f not x::xs or x::[] \n");
                    None)
              else (
                prerr_string
                  (string_of_positions r.pos ^ "| hd func not same type p e["
                  ^ string_of_type (List.hd f)
                  ^ ":" ^ string_of_type p ^ "] \n");
                None)
          | t1, t2 ->
              prerr_string
                (string_of_positions r.pos ^ "|none_Arrow:p = ["
               ^ string_of_type t1 ^ "] [" ^ string_of_type t2 ^ "] \n");
              None)
      | Some t1, None ->
          prerr_string
            (string_of_positions r.pos ^ "|Func:none_Param = ["
           ^ string_of_expr t1 ^ "]\n");
          None
      | None, Some t2 ->
          prerr_string
            (string_of_positions r.pos ^ "|none_Func:Param = ["
           ^ string_of_expr t2 ^ "]\n");
          None
      | None, None ->
          prerr_string (string_of_positions r.pos ^ "|none_Func:none_Param \n");
          None)
  | BinExpr ({ op = Application; _ } as r) -> (
      match (typecheck_expr env r.left, typecheck_expr env r.right) with
      | Some func', Some param' -> (
          print_string (string_of_type (prop_of_expr func') ^ ":BinExpr a \n");
          print_string (string_of_type (prop_of_expr param') ^ ":BinExpr b \n");
          match (prop_of_expr func', prop_of_expr param') with
          | Arrow f, p ->
              if is_same_type (List.hd f) p then (
                match f with
                | x :: [] ->
                    (* FUNC-APP-L *)
                    print_string
                      (string_of_positions r.pos
                     ^ "|BinExpr FuncAppWithParam x::[] create \n");
                    Some
                      (BinExpr { r with prop = x; left = func'; right = param' })
                | _ :: xs ->
                    print_string
                      (string_of_positions r.pos
                     ^ "|BinExpr FuncAppWithParam x::xs  create \n");
                    Some
                      (BinExpr
                         {
                           r with
                           (* prop = List.hd xs; *)
                           prop = Arrow xs;
                           left = func';
                           right = param';
                         })
                | _ ->
                    prerr_string
                      (string_of_positions r.pos
                     ^ "|BinExpr f not x::xs or x::[] \n");
                    None)
              else (
                prerr_string
                  (string_of_positions r.pos
                 ^ "|BinExpr hd func not same type p e["
                  ^ string_of_type (List.hd f)
                  ^ ":" ^ string_of_type p ^ "] \n");
                None)
          | t1, t2 ->
              prerr_string
                (string_of_positions r.pos ^ "|BinExpr none_Arrow:p = ["
               ^ string_of_type t1 ^ "] [" ^ string_of_type t2 ^ "] \n");
              None)
      | _ -> None)
  | _ ->
      prerr_string ": not implemented yet, or somethings are wrong\n";
      None

let typecheck_defn (env : Env.tbl) (definitions : unit defn) : bool =
  match definitions with
  | FuncDef (pos, _, args, ret_type, body) -> (
      match typecheck_args pos (Env.copy env) args with
      | Some (new_env, _) -> (
          match
            (typecheck_typeexpr new_env ret_type, typecheck_expr new_env body)
          with
          | Some rt, Some body' ->
              let t1, t2 = (prop_of_typeexpr rt, prop_of_expr body') in
              if is_same_type t1 t2 then true
              else (
                prerr_string
                  (string_of_positions pos
                 ^ ": expected the body of function to return the same type, \
                    expect " ^ string_of_type t1 ^ " but got "
                 ^ string_of_type t2 ^ "\n");
                false)
          | _ -> false)
      | _ -> false)
  | TypeDef _ -> false
  | DataDef _ -> false

let typecheck_prog (env : Env.tbl) (p : unit prog) : bool =
  match p with
  | Program (_, _, []) -> true
  | Program (_, _, defns) ->
      List.fold_left ( && ) true (List.map (typecheck_defn env) defns)

let init_env_defns (parent_pos : pos) (env : Env.tbl option) (def : unit defn) :
    Env.tbl option =
  match env with
  | Some env -> (
      match def with
      | FuncDef (pos, name, args, ret_type, _) -> (
          match
            ( typecheck_args pos (Env.copy env) args,
              typecheck_typeexpr env ret_type )
          with
          | Some (_, args'), Some rt -> (
              match Env.lookup env (s_name_alrin parent_pos name) with
              | Some _ ->
                  prerr_string
                    (string_of_positions pos ^ ": duplicated function \"" ^ name
                   ^ "\" \n");
                  None
              | None ->
                  let f_type =
                    curry_ver (args_to_langtypes args') (prop_of_typeexpr rt)
                  in
                  Env.add env (s_name_alrin parent_pos name) f_type;
                  Env.replace env name f_type;
                  Some env)
          | _ -> None)
      | _ -> Some env)
  | None -> None

let init_env_prog (p : unit prog) (env : Env.tbl) : Env.tbl option =
  match p with
  | Program (_, [], []) -> Some env
  | Program (pos, _, defns) ->
      List.fold_left (init_env_defns pos) (Some env) defns

let init_env (p : unit prog) : Env.tbl option = init_env_prog p (Env.default ())

(* This func will be removed later. Used for debugging first *)
let string_of_hashtbl_value (s : lang_type) : string = string_of_type s

let typecheck (p : unit prog) : bool =
  match init_env p with
  | Some env ->
      (* Hashtbl.iter
           (fun key value ->
             Printf.printf "%s -: %s\n" key (string_of_hashtbl_value value))
           env;
         print_newline (); *)
      typecheck_prog env p
  | None -> false

(* CpecmuCompiler.A3_typechecking.typecheck_file "./src/main.cpe";; *)
(* CpecmuCompiler.A2_parsing.parse_file "./src/main.cpe";; *)
