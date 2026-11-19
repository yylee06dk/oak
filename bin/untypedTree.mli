module Op = Operator
module OV = OakValues
module OT = OakTypes

(**
  Untyped trees are built with boxed(dynamic) values which holds type info within.
*)
type t =
  | Literal of OV.t
  | Identifier of string
  | BinOp of t * Op.t * t
  | VarDecl of t * OT.t * t * t
  | RecFunc of t * (t * OT.t) * OT.t * t * t
  | AnonFunc of (t * OT.t) * t
  | App of t * t
  | If of t * t * t

val pp : Format.formatter -> t -> unit

val show : t -> string