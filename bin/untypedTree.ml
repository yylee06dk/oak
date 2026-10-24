type t =
  | Literal of Box.t
  | Term of t * t
  [@@deriving show]
