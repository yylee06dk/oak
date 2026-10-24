module Utt = UntypedTree
module Tt = TypedTree

(* returns the targeting typed tree along with the constructor containing the resulting type info *)
type t =
  | IntType of int Tt.t
  | FloatType of float Tt.t
  
exception TypeMismatch of string

(* Traverses the tree to check type correctness *)
let rec typeCheck utt =
  let open Utt in
  match utt with
  | Literal (Box.Int n) ->
    IntType (Tt.IntLiteral n)
  | Literal (Box.Float f) -> 
    FloatType (Tt.FloatLiteral f)
  | BinOp (l,op,r) ->
    let lRes = typeCheck l in
    let rRes = typeCheck r in
    match lRes, rRes with
    | IntType ltree, IntType rtree -> 
      IntType (Tt.BinOpInt (ltree, op, rtree))
    | FloatType ltree, FloatType rtree -> 
      FloatType (Tt.BinOpFloat (ltree, op, rtree))
    | _, _-> 
      raise (TypeMismatch "Addition operator's operands both should have type \'int\' or \'float\'") 
  
