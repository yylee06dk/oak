module Op = Operator
module OV = OakValues
module Tt = TypedTree
module Env = Map.Make(String)

exception InternalTCErr

(* Uses locally abstract types to deal with each branch resulting in different types *)
let rec interp = fun tree env ->
  match tree with
  | Tt.Literal value -> value
  | Tt.BinOpInt (l,op,r) | Tt.BinOpFloat (l, op, r) | Tt.BinOpBool (l, op, r) -> 
    OV.do_binOp op (interp l env) (interp r env)
  | Tt.VarDeclCheck (name, init_val, scope) ->
    let new_env = Env.add name (interp init_val env) env in
    interp scope new_env
  | Tt.Identifier name_string ->
    begin match Env.find_opt name_string env with
    | Some x -> x
    | None -> failwith "Unbound variable"
    end
  | Tt.AnonFuncCheck (arg_list, body) ->
    

