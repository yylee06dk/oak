module Utt = UntypedTree
module Tt = TypedTree

(* returns the targeting typed tree along with the constructor containing the resulting type info *)
type t =
  | IntType of int Tt.t
  | FloatType of float Tt.t
  
exception TypeMismatch of string

(* Traverses the tree to check type correctness *)
let rec typeCheck utt =
  match utt with
  | Utt.Literal box ->
    (match box with
    | Box.Int n -> IntType (Tt.IntLiteral n)
    | Box.Float f -> FloatType (Tt.FloatLiteral f))
  | Utt.BinOp (l,op,r) ->
    let ltree = typeCheck l in
    let rtree = typeCheck r in
      match ltree, rtree with
      | IntType ltt, IntType rtt -> IntType (Tt.BinOpInt (ltt, op, rtt))
      | FloatType ltt, FloatType rtt -> FloatType (Tt.BinOpFloat (ltt, op, rtt))
      | _, _-> raise (TypeMismatch "Addition operator's operands both should have type \'int\' or \'float\'") 
  
