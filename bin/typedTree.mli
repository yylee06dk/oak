module Op = Operator
module OV = OakValues
module OT = OakTypes

type t =
  | Literal of OV.t
  | Identifier of string
  | BinOpInt of t * Op.t * t
  | BinOpFloat of t * Op.t * t
  | BinOpBool of t * Op.t * t
  | VarDeclCheck of string * t * t
  | AnonFuncCheck of string list * t
  [@@deriving show]
