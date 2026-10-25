module Utt = UntypedTree
module Tt = TypedTree

(** represents what the expression(AST) should interpret to.
    Also, carries information about the resulting type checked tree *)
type t =
  | IntType of int Tt.t
  | FloatType of float Tt.t
  | BoolType of bool Tt.t
  
(** raised when type mismatch is found *)
exception TypeMismatch of string
    
(** checks the type correctness of the given untyped tree
    and returns a typed tree when correctness is ensured along with info about what value it results in
    raises [TypeMismatch] exception when typeCheck fails *)
val typeCheck : Utt.t -> t