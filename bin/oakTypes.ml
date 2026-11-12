type t =
  | Int
  | Float
  | Bool
  | Char
  | String
  | Arrow of t * t
  [@@deriving show]
