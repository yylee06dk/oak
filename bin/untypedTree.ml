module Op = Operator
module OV = OakValues
module OT = OakTypes

type t =
  | Literal of OV.t
  | Identifier of string
  | BinOp of t * Op.t * t
  | VarDecl of t * OT.t * t * t
  [@@deriving show]
