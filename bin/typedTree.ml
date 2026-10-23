type tt =
  | IntLiteral of int
  | FloatLiteral of float
  | TermInt of tt * tt
  | TermFloat of tt * tt
[@@deriving show]
