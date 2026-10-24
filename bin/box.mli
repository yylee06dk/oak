(** 
    Used to represent untyped trees' literal nodes at compile-time
    Exposed constructors below tells users(implementor of the interpreter) about type info.
    Also, ppx-show is derived for pretty printing
*)
type t =
  | BoxInt of int
  | BoxFloat of float
  [@@deriving show]