module Tt = TypedTree
open Box

let rec interp : type a. a Tt.t -> a = function
  | Tt.IntLiteral n -> n
  | Tt.FloatLiteral f -> f
  | Tt.AddInt (l,r) -> (interp l) + (interp r)
  | Tt.AddFloat (l,r) -> (interp l) +. (interp r)

