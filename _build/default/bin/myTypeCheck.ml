module Utt = UntypedTree
module Tt = TypedTree

type typeEnum =
  | Integer
  | Float

let rec typeCheck utt =
  match utt with
  | Utt.Valuen n -> (Tt.IntLiteral n, Integer)
  | Utt.Valuef f -> (Tt.FloatLiteral f, Float)
  | Utt.Term (l,r) ->
    let (ltt, lt) = typeCheck l in
    let (rtt, rt) = typeCheck r in
      match lt, rt with
      | Integer, Integer -> (Tt.TermInt (ltt, rtt), Integer)
      | Float, Float -> (Tt.TermFloat (ltt, rtt), Float)
      | _, _-> (Tt.IntLiteral 1, Integer) (*change this to raising exception*)
  
