open Lexer
open Parser

type value = 
  |ValNumber of int
  |ValString of string
  |ValBool of bool
  |ValFunction of ast * ast
  |ValReturn of value
  |ValScope of (string, value) Hashtbl.t list
  |ValList of value list
  |ValDict of (value, value) Hashtbl.t
  |ValNone

let string_of_value = function 
  |ValNumber _ -> "Number"
  |ValString _ -> "String"
  |ValBool _ -> "Bool"
  |ValFunction _ -> "Function"
  |ValReturn _ -> "Return"
  |ValScope _ -> "Scope"
  |ValList _ -> "List"
  |ValDict _ -> "Dict"
  |ValNone _ -> "None"

let int_of_valnumber = function
  |ValNumber n -> n
  |_ -> raise (Failure "Invalid type")
let string_of_valstring = function
  |ValString s -> s
  |_ -> raise (Failure "Invalid type")
let bool_of_valbool = function
  |ValBool b -> b
  |_ -> raise (Failure "Invalid type")

let pow x = function
  |0 -> 1
  |1 -> x
  |y -> (
    let rec pow_i acc = function
      |1 -> acc
      |n -> pow_i (acc*x) (n-1)
    in pow_i x y
  )

let rec eval ?(path_prefix="") ?(default_assignments=[Hashtbl.create 100]) ast =
  let acc = ref "" in
  let rec truthy = function
    |ValNumber n when n <> 0 -> true
    |ValNumber _ -> false
    |ValString s when s <> "" -> true
    |ValString _ -> false
    |ValBool b -> b
    |ValFunction _ -> true
    |ValNone -> false
    |_ -> false
  and get_assigned name assignments =
    let rec get_assigned_i (hd::tl) =
      match Hashtbl.find_opt hd name with
        | Some v -> v
        | None -> (
          if tl == [] then ValNone
          else get_assigned_i tl
        )
    in get_assigned_i assignments
  and set_assigned ?(must_exist=true) name rhs assignments =
    let rec attempt_replace (hd::tl) =
      if not must_exist then false else (
        match Hashtbl.find_opt hd name with
          | Some v -> (Hashtbl.replace hd name rhs; true)
          | None -> (
            if tl == [] then false
            else attempt_replace tl
          )
      )
    in
    if attempt_replace assignments <> true then (
      if must_exist then (
        raise (Failure ("Tried to assign variable " ^ name ^ " (doesn't exist)"))
      ) else (
        let (hd::_) = assignments in
        Hashtbl.add hd name rhs
      )
    )
  and eval_i assignments = function
    |OpInsertRaw r -> (acc := !acc ^ r; ValNone)
    |OpSymb s -> get_assigned s assignments
    |OpString s -> ValString s
    |OpNumber n -> ValNumber n
    |OpList l -> ValList (List.map (fun i -> eval_i assignments i) l)
    |OpDict l -> (
      let dict = Hashtbl.create 100 in
      List.iter (fun (OpDictMapping (lhs, rhs)) -> Hashtbl.add dict (eval_i assignments lhs) (eval_i assignments rhs)) l;
      ValDict dict
    )
    |OpAdd (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) + (eval_i assignments rhs |> int_of_valnumber))
    |OpSub (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) - (eval_i assignments rhs |> int_of_valnumber))
    |OpMul (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) * (eval_i assignments rhs |> int_of_valnumber))
    |OpDiv (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) / (eval_i assignments rhs |> int_of_valnumber))
    |OpPow (lhs, rhs) -> ValNumber (pow (eval_i assignments lhs |> int_of_valnumber) (eval_i assignments rhs |> int_of_valnumber))
    |OpNegate u -> ValNumber(-(eval_i assignments u |> int_of_valnumber))
    |OpIfElse (comparison, ifblock, elseblock) ->
      if (truthy (eval_i assignments comparison)) then
        eval_i assignments ifblock 
      else 
        eval_i assignments elseblock
    |OpFor (init, comparison, after, block) -> (
      eval_i assignments init;
      while (truthy (eval_i assignments comparison)) do
        eval_i assignments block;
        eval_i assignments after;
      done;
      ValNone
    )
    |OpWhile (comparison, block) -> (
      while (truthy (eval_i assignments comparison)) do
        eval_i assignments block;
      done;
      ValNone
    )
    |OpStatementList sl -> (
      let rec statement_list_i = function
        |hd::tl -> (
          match eval_i assignments hd with
            |ValReturn _ as v -> v
            |_ -> statement_list_i tl
        )
        |[] -> ValNone
      in statement_list_i sl
    )
    |OpBlock ast -> eval_i ((Hashtbl.create 100)::assignments) ast
    |OpFunDef (OpSymb name, arglist, block) -> (set_assigned ~must_exist:false  name (ValFunction (arglist, block)) assignments; ValNone)
    |OpFunCall (OpSymb name, OpParamList paramlist) -> (
      match name with
        |"print" -> (
          let rec print_i = function
            |(hd::[]) -> (
              acc := (!acc) ^ (match (eval_i assignments hd) with
                |ValString s -> s
                |ValNumber n -> string_of_int n
                |ValBool b -> string_of_bool b
                |_->""
              );
              ValNone
            )
            |(hd::tl) -> (
              acc := (!acc) ^ (match (eval_i assignments hd) with
                |ValString s -> s
                |ValNumber n -> string_of_int n
                |ValBool b -> string_of_bool b
                |_->""
              ) ^ " ";
              print_i tl
            )
          in print_i paramlist
        )
        |_ -> (
          match (get_assigned name assignments) with 
            |ValFunction (OpArgList arglist, funblock) -> (
              let newScope = [Hashtbl.create 100] in
              let rec build_scope_i = function 
                |([], []) -> ()
                |([], _) -> raise (Failure ("Not enough arguments for function " ^ name))
                |(_, []) -> raise (Failure ("Too many arguments for function " ^ name))
                |((OpSymb arg)::atl, param::ptl) -> (
                  set_assigned ~must_exist:false arg (eval_i assignments param) newScope;
                  build_scope_i (atl,ptl)
                )
              in build_scope_i (arglist, paramlist);
              match eval_i (newScope@assignments) funblock with
                | ValReturn v -> v
                | _ -> ValNone
            )
            |ValNone -> raise (Failure ("No function exists called " ^ name))
            |_ -> raise (Failure ("Type is not callable"))
        )
    )
    |(OpAssign _) as node -> (
      let rec transform_tree_i rv = function
        |OpAssign (OpSymb name, rhs) -> (name, rhs)
        |OpAssign (lhs, rhs) -> transform_tree_i rhs lhs
        |OpAccess ((OpSymb name) as lhs, rhs) -> (name, OpRep (lhs, rhs, rv))
        |OpAccess (lhs, rhs) -> transform_tree_i (OpRep (lhs, rhs, rv)) lhs
      in
      let (name, transformed) = transform_tree_i OpNop node in
      let v = eval_i assignments transformed in
      set_assigned name v assignments;
      v
    )
    |OpLetAssign (OpSymb name, rhs) -> (
      let v = eval_i assignments rhs in
      set_assigned ~must_exist:false name v assignments;
      v
    )
    |OpRep (source, dest, v) -> (
      match ((eval_i assignments source), (eval_i assignments dest)) with
        |(ValList l, ValNumber pos) -> (ValList (List.mapi (fun i x -> if i = pos then (eval_i assignments v) else x) l))
        |(ValDict d, k) -> (Hashtbl.replace d k (eval_i assignments v); ValDict d)
        |_ -> raise (Failure "Invalid type for indexed assign")
    )
    |OpDot (lhs, rhs) -> (
      match (eval_i assignments lhs) with 
        |ValScope s -> (
          eval_i (s@assignments) rhs
        )
        |_ -> raise (Failure "Invalid type for dot operator")
    )
    |OpAccess (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValString s, ValNumber n) -> ValString (string_of_char s.[n])
        |(ValList l, ValNumber n) -> List.nth l n
        |(ValDict d, v) -> (
          match (Hashtbl.find_opt d v) with
            |Some v -> v
            |None -> ValNone
        )
        |(l,r) -> raise (Failure ("Invalid combination for access: " ^ (string_of_value l) ^ " and " ^ (string_of_value r)))
    )
    |OpEquiv (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a = b)
        |(ValString a, ValString b) -> ValBool (a = b)
        |(ValBool a, ValBool b) -> ValBool (a = b)
        |_ -> ValNone
    )
    |OpNotEquiv (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a <> b)
        |(ValString a, ValString b) -> ValBool (a <> b)
        |(ValBool a, ValBool b) -> ValBool (a <> b)
        |_ -> ValNone
    )
    |OpLT (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a < b)
        |_ -> ValNone
    )
    |OpGT (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a > b)
        |_ -> ValNone
    )
    |OpLTE (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a <= b)
        |_ -> ValNone
    )
    |OpGTE (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a >= b)
        |_ -> ValNone
    )
    |OpNop -> ValNone
    |OpReturn u -> ValReturn (eval_i assignments u)
    |OpIncYield u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n+1)) assignments; ValNumber (n+1))
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpYieldInc u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n+1)) assignments; cv)
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpDecYield u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n-1)) assignments; ValNumber (n-1))
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpYieldDec u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n-1)) assignments; cv)
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpProgram u -> (
      eval_i assignments u;
      (match (get_assigned "layout" assignments) with
        | ValString s -> (
          let new_scope = [Hashtbl.create 100] in
          set_assigned ~must_exist:false "page" (ValScope assignments) new_scope;
          set_assigned ~must_exist:false "content" (ValString !acc) new_scope;
          let layout_path = "source/_layouts/"^s^".html" in
          if Sys.file_exists layout_path then (
            let ast = parsefile layout_path in
            acc := eval ~default_assignments:new_scope ~path_prefix:"source/_layouts/" ast;
          )
        )
        | ValNone -> ());
      ValNone
    )
    |OpInclude (OpString s) -> (
      let path = path_prefix ^ s in
      if Sys.file_exists path then (
        acc := (!acc) ^ (eval ~path_prefix:path_prefix (parsefile path))
      );
      ValNone
    )
    |OpForEach (OpSymb k, v, body) -> (
      (match (eval_i assignments v) with
        |ValList l -> List.iter (fun x -> set_assigned ~must_exist:false k x assignments; ignore(eval_i assignments body)) l
        |_ -> raise (Failure "Invalid type for foreach"));
      ValNone
    )
    |OpForEachKV (OpSymb k, OpSymb v, vv, body) -> (
      (match (eval_i assignments vv) with
        |ValDict d -> Hashtbl.iter (fun x y -> set_assigned ~must_exist:false k x assignments; set_assigned v y assignments; ignore (eval_i assignments body)) d
        |_ -> raise (Failure "Invalid type for foreach"));
      ValNone
    )
    |_ -> raise (Failure "Unimplemented")
  in eval_i default_assignments ast;
  !acc
and parsefile fn =
  let ic = open_in fn in
  let length = in_channel_length ic in
  let buffer = Bytes.create length in
  really_input ic buffer 0 length;
  let program = Bytes.to_string buffer in

  program |> lex |> parse

let join ?(exclude_last=false) ?(joiner="") l =
  let rec join_i acc = function
    |hd::[] when exclude_last -> acc
    |hd::tl when acc = "" -> join_i (hd) tl
    |hd::tl -> join_i (acc^joiner^hd) tl
    |[] -> acc
  in join_i "" l

let () = 
  if Sys.file_exists "source" then (
    if not (Sys.is_directory "source") then
      raise (Failure "'source' exists and is not a directory!");
  ) else
    raise (Failure ("Please create a 'source' directory to use " ^ Sys.argv.(0)));

  if Sys.file_exists "output" then (
    if Sys.is_directory "output" then (
      let rec remove_rec = function
        | f::fs when Sys.is_directory f ->
          Sys.readdir f
            |> Array.to_list
            |> List.map (Filename.concat f)
            |> List.append fs
            |> remove_rec;
          Unix.rmdir f
        | f::fs -> (Sys.remove f; remove_rec fs)
        | [] -> ()
      in
        remove_rec ["output"]
    ) else (
      raise (Failure "'output' exists and is not a directory!")
    )
  );

  let rec gen = function
    | f::fs when (Sys.is_directory f) && ((Filename.basename f).[0] <> '_') ->
      Sys.readdir f
        |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs
        |> gen
    | f::fs when (Filename.basename f).[0] <> '_' -> (
      let path_list = String.split_on_char '/' f in
      let result = eval ~path_prefix:((join ~exclude_last:true ~joiner:"/" path_list) ^ "/") (parsefile f) in
      let (_::path) = path_list in
      let output_path = "output"::path in
      let rec create_path_if_not_exist prefix = function
        |(to_create::tail) when tail <> [] -> (
          let to_check = prefix ^ to_create in
          if Sys.file_exists to_check then (
            if Sys.is_directory to_check then 
              create_path_if_not_exist (to_check ^ "/") tail
            else
              raise (Failure ("Could not create directory " ^ to_check))
          ) else (
            Unix.mkdir to_check 0o777;
            create_path_if_not_exist (to_check ^ "/") tail
          )
        )
        |_ -> ()
      in create_path_if_not_exist "" output_path;
      let asbytes = Bytes.of_string result in
      let bytes_length = Bytes.length asbytes in
      let oc = open_out (join ~joiner:"/" output_path) in
      output oc asbytes 0 bytes_length;
      print_endline @@ "Output: " ^ (join ~joiner:"/" output_path);
      gen fs
    )
    | f::fs -> gen fs
    | [] -> ()
  in gen ["source"];
  print_endline "Done!"