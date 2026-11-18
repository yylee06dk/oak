module Op = Operator
module OV = OakValues
module OT = OakTypes

type t = OakValues.typed_tree =
  | Literal of OV.t
  | Identifier of string
  | BinOpInt of t * Op.t * t
  | BinOpFloat of t * Op.t * t
  | BinOpBool of t * Op.t * t
  | VarDeclCheck of string * t * t
  | AnonFuncCheck of string * t
  | AppCheck of t * t
  | IfCheck of t * t * t

val pp : Format.formatter -> t -> unit

val show : t -> string
