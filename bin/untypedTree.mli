module Op = Operator

(** 
  Used to represent untyped trees' literal(value) nodes at compile-time
  Exposed constructors below tells users(implementor of the interpreter) about type info.
  Also, ppx-show is derived for pretty printing
*)
module Box : sig 
  type t =
    | Int of int
    | Float of float
    | Bool of bool
    [@@deriving show]
end

(**
  Untyped trees are built with boxed(dynamic) values which holds type info within.
*)
type t =
  | Literal of Box.t
  | BinOp of t * Op.t * t
  [@@deriving show]