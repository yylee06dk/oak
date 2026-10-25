(*
  Arithmetic operators.
  Lives in a separate module since operators are used all around the code
*)
type t = 
  | Plus
  | Minus
  | Mult
  | Div
  | And
  | Or
  [@@deriving show { with_path = false }] 
  
exception UnknownOperator of string