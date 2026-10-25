module Op = Operator

type _ t =
  | IntLiteral : int -> int t 
  | BinOpInt : int t * Op.t * int t -> int t
  | FloatLiteral : float -> float t
  | BinOpFloat : float t * Op.t * float t -> float t
  | BoolLiteral : bool -> bool t
  | LogicOpBool : bool t * Op.t * bool t -> bool t

val show : 'a t -> string