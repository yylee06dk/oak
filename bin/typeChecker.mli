module Utt = UntypedTree
module Tt = TypedTree
module OT = OakTypes

(** represents what the expression(AST) should interpret to.
    Also, carries information about the resulting type checked tree's type 
    Uses Oaktypes to have a uniform interface of types in the codebase *)
type t = 
  | Check of OT.t * Tt.t
  [@@deriving show]
  
module Context : Map.S with type key = string

  
(** raised when type mismatch is found *)
exception TypeMismatch of string
    
(** checks the type correctness of the given untyped tree
    and returns a typed tree when correctness is ensured along with info about what value it results in
    raises [TypeMismatch] exception when typeCheck fails *)
val type_check : Utt.t -> OT.t Context.t -> t