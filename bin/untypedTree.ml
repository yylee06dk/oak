module Op = Operator
type t =
  | Literal of Box.t
  | BinOp of t * Op.t * t
  [@@deriving show]
