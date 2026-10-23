module Tt = TypedTree

type box =
  | Boxn of int
  | Boxf of float

let addn b1 b2 =
  match b1, b2 with
  | Boxn n1, Boxn n2 -> Boxn (n1 + n2)
  | _, _ -> assert false

let addf b1 b2 =
  match b1, b2 with
  | Boxf f1, Boxf f2 -> Boxf (f1 +. f2)
  | _, _ -> assert false

let rec interp : Tt.tt -> box =
  function
  | Tt.IntLiteral n -> Boxn n
  | Tt.FloatLiteral f -> Boxf f
  | Tt.TermInt (l,r) -> addn (interp l) (interp r)
  | Tt.TermFloat (l,r) -> addf (interp l) (interp r)

