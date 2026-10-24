(*
  Arithmetic operators.
  Lives in a separate module since operators are used all around the code
*)
type t = 
  | Plus
  | Minus
  | Mult
  | Div
  [@@deriving show { with_path = false }] 
  
exception UnknownOperator of string
  
let read op =
  match op with
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Mult
  | "/" -> Div
  | unknownOp -> raise (UnknownOperator unknownOp)
