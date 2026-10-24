module Op = Operator

type _ t =
  | IntLiteral : int -> int t 
  | FloatLiteral : float -> float t
  | BinOpInt : int t * Op.t * int t -> int t
  | BinOpFloat : float t * Op.t * float t -> float t

val show : 'a t -> string