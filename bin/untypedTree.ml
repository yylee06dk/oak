module Op = Operator

module Box = struct 
  type t =
    | Int of int
    | Float of float
    [@@deriving show]
end

type t =
  | Literal of Box.t
  | BinOp of t * Op.t * t
  [@@deriving show]
