module Op = Operator
module Utt = UntypedTree
module Tt = TypedTree

(* returns the targeting typed tree along with the constructor containing the resulting type info *)
type t =
  | IntType of int Tt.t
  | FloatType of float Tt.t
  | BoolType of bool Tt.t
  
exception TypeMismatch of string

let operator_types =
  [
    (Op.Plus, ["int"; "float"]);
    (Op.Minus, ["int"; "float"]);
    (Op.Mult, ["int"; "float"]);
    (Op.Div, ["int"; "float"]);

    (Op.And, ["bool"]);
    (Op.Or, ["bool"]);
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
let rec typeCheck utt =
  let open Utt in
  match utt with
  | Literal (Box.Int n) ->
    IntType (Tt.IntLiteral n)
  | Literal (Box.Float f) -> 
    FloatType (Tt.FloatLiteral f)
  | Literal (Box.Bool b) ->
    BoolType (Tt.BoolLiteral b)
  | BinOp (l,op,r) ->
    let lRes = typeCheck l in
    let rRes = typeCheck r in
    match lRes, rRes with
    | IntType ltree, IntType rtree -> 
      IntType (Tt.BinOpInt (ltree, op, rtree))
    | FloatType ltree, FloatType rtree -> 
      FloatType (Tt.BinOpFloat (ltree, op, rtree))
    | BoolType ltree, BoolType rtree ->
      BoolType (Tt.LogicOpBool (ltree, op, rtree))
    | _, _-> 
      raise (TypeMismatch "Addition operator's operands both should have type \'int\' or \'float\'") 
  
