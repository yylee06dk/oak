module Env : Map.S with type key = string

(** Means an internal bug. Raised when the invariant that type checker rejects all ill-typed code *)
exception InternalTCErr

(** given a typed tree, interprets it by walking the tree *)
val interp : TypedTree.t -> OakValues.t Env.t ref -> OakValues.t