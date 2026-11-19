module Op = Operator
module Utt = UntypedTree
module Tt = TypedTree
module OV = OakValues
module OT = OakTypes
module Context = struct
  include Map.Make(String)
  let pp pp_value fmt env =
    Format.fprintf fmt "Context:{@[<hov>";
    iter (fun key value ->
      Format.fprintf fmt "@ %s = %a;" key pp_value value)
      env;
    Format.fprintf fmt "@] }"
    
  let show ctxt =
    Format.asprintf "%a" (pp OT.pp) ctxt
end

type t = 
  | Check of OT.t * Tt.t
  [@@deriving show]
  
type context =
  OakTypes.t Context.t

exception TypeMismatch of string

let operator_types =
  [
    (Op.Plus, ["int"; "float"]);
    (Op.Minus, ["int"; "float"]);
    (Op.Mult, ["int"; "float"]);
    (Op.Div, ["int"; "float"]);

    (Op.And, ["bool"]);
    (Op.Or, ["bool"]);

    (Op.Eq, ["int"; "float"; "bool"; "char"; "string"]);
    (Op.Neq, ["int"; "float"; "bool"; "char"; "string"]);
    (Op.Great, ["int"; "float"; "char"; "string"]);
    (Op.Geq, ["int"; "float"; "char"; "string"]);
    (Op.Less, ["int"; "float"; "char"; "string"]);
    (Op.Leq, ["int"; "float"; "char"; "string"]);
  ]

let generate_type_mismatch op =
  let poss_types = List.assoc op operator_types in
  let errmsg = 
    Format.asprintf "%a operator's operands both should have type: %s"
      Op.pp op 
      (String.concat " or " poss_types)
  in
  TypeMismatch errmsg

(* Traverses the tree to check type correctness *)
let rec type_check : Utt.t -> context -> t =
  fun utt context -> let open Utt in
  match utt with
  | Literal v ->
    OV.(match v with
    | Int n -> Check (OT.Int, Tt.Literal v)
    | Float n -> Check (OT.Float, Tt.Literal v)
    | Bool n -> Check (OT.Bool, Tt.Literal v)
    | Char n -> Check (OT.Char, Tt.Literal v)
    | String n -> Check (OT.String, Tt.Literal v)
    | Closure (_,_,_) -> assert false)
  | Identifier i ->
    let type_found = match Context.find_opt i context with
      | Some x -> x
      | None -> failwith @@ "Unbounded variable: " ^ i ^ " in " ^ Context.show context
    in  
    Check (type_found, Tt.Identifier i)
  | BinOp (l,op,r) ->
    let Check (ltype, ltree) = type_check l context in
    let Check (rtype, rtree) = type_check r context in
    begin match op with
    | Op.And | Op.Or ->
      (match ltype, rtype with 
        | OT.Bool, OT.Bool ->
          Check (OT.Bool, Tt.BinOpBool (ltree, op, rtree))
        | _, _-> 
          raise @@ generate_type_mismatch op)
    | Op.Plus | Op.Minus | Op.Mult | Op.Div ->
      (match ltype, rtype with
        | OT.Int, OT.Int -> Check (OT.Int, Tt.BinOpInt (ltree, op, rtree))
        | OT.Float, OT.Float -> Check (OT.Float, Tt.BinOpFloat (ltree, op, rtree))
        | _, _-> 
          raise @@ generate_type_mismatch op)
    | Op.Eq | Op.Neq ->
      (match ltype, rtype with
        | OT.Int, OT.Int
        | OT.Float, OT.Float
        | OT.Bool, OT.Bool
        | OT.Char, OT.Char
        | OT.String, OT.String ->
          Check (OT.Bool, Tt.BinOpBool (ltree, op, rtree))
        | _, _ ->
          raise @@ generate_type_mismatch op)
    | Op.Great | Op.Geq | Op.Less | Op.Leq ->
      (match ltype, rtype with
        | OT.Int, OT.Int
        | OT.Float, OT.Float
        | OT.Char, OT.Char
        | OT.String, OT.String ->
          Check (OT.Bool, Tt.BinOpBool (ltree, op, rtree))
        | _, _ ->
          raise @@ generate_type_mismatch op)
    end
  | VarDecl (name, type_note, init_val, scope) ->
    let name_string = 
      match name with | Utt.Identifier s -> s | _ -> failwith "Expected Identifier" 
    in
    let new_context =
      Context.add name_string type_note context
    in
    let Check (init_type, init_tree) = type_check init_val context in
    let Check (scope_type, scope_tree) = type_check scope new_context in
    if init_type <> type_note then failwith 
      ("Given value different from annotated type: " 
      ^ OT.show init_type ^ " / "
      ^ OT.show type_note ^ " / "
      ^ Tt.show init_tree)
    else Check (scope_type, Tt.VarDeclCheck (name_string, init_tree, scope_tree))
  | RecFunc (name, (arg, arg_type), dest_type, body, scope) ->
    let (name_str, arg_name) =
      match name, arg with
      | Identifier i, Identifier a -> (i,a)
      | _ -> failwith "Expected Identifier in rec fun"
    in
    let func_type = OT.Arrow (arg_type, dest_type) in
    let scope_context =
      Context.add name_str func_type context
    in
    let body_context =
      Context.add arg_name arg_type scope_context
    in
    let Check (body_type, body_tree) =
      type_check body body_context
    in
    let Check (scope_type, scope_tree) =
      type_check scope scope_context
    in
    if body_type <> dest_type then raise @@ TypeMismatch "rec fun's type annotation doesn't match the actual type."
    else Check (scope_type , Tt.RecFuncCheck (name_str, arg_name, body_tree, scope_tree))

  | AnonFunc (arg, body) ->
    let (arg_name, arg_type) =
      match fst arg with
      | Identifier i -> (i, snd arg)
      | _ -> failwith "Expected Identifier in anon fun"
    in
    let new_context =
      Context.add arg_name arg_type context
    in
    let Check (body_type, body_tree) =
      type_check body new_context
    in
    let func_type =
      OT.Arrow (arg_type, body_type)
    in
    Check (func_type, Tt.AnonFuncCheck (arg_name, body_tree))
  | App (f, param) ->
    let Check (func_type, func_tree) =
      type_check f context
    in
    let Check (param_type, param_tree) =
      type_check param context
    in
    let res_type =
      match func_type with
      | OT.Arrow (src, dest) -> 
        if src = param_type then dest
        else raise (TypeMismatch "Function type mismatch")
      | _ -> raise @@ TypeMismatch "Uncallable function given"
    in
    Check (res_type, Tt.AppCheck (func_tree, param_tree))
  | If (cond, tb, fb) ->
    let Check (cond_type, cond_tree) =
      type_check cond context
    in
    let Check (tb_type, tb_tree) =
      type_check tb context
    in
    let Check (fb_type, fb_tree) =
      type_check fb context
    in

    if cond_type <> OT.Bool then raise @@ TypeMismatch "If should have boolean typed condition"
    else
      if tb_type <> fb_type then raise @@ TypeMismatch "If's both branch should have same type"
      else Check (tb_type, IfCheck (cond_tree, tb_tree, fb_tree))
