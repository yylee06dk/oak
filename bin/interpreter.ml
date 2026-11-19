module Op = Operator
module OV = OakValues
module OT = OakTypes
module Tt = TypedTree
module Env = struct 
  include Map.Make(String)
  let pp pp_value fmt env =
    Format.fprintf fmt "Env:{@[<hov>";
    iter (fun key value ->
      Format.fprintf fmt "@ %s = %a;" key pp_value value)
      env;
    Format.fprintf fmt "@] }"
    
  let show env =
    Format.asprintf "%a" (pp OV.pp) env
end

exception InternalTCErr

(* Uses locally abstract types to deal with each branch resulting in different types *)
let rec interp = fun tree env ->
  match tree with
  | Tt.Literal value -> value
  | Tt.BinOpInt (l,op,r) | Tt.BinOpFloat (l, op, r) | Tt.BinOpBool (l, op, r) -> 
    OV.do_binOp op (interp l env) (interp r env)
  | Tt.Identifier name_string ->
    begin match Env.find_opt name_string !env with
    | Some x -> x
    | None -> failwith @@ "Unbound variable: " ^ name_string ^ " in " ^ Env.show !env
    end
  | Tt.VarDeclCheck (name, init_val, scope) ->
    let new_env = ref @@ Env.add name (interp init_val env) !env in
    interp scope new_env
  | Tt.RecFuncCheck (name, arg, body, scope) ->
    let new_env = ref @@ Env.add name (OV.Int 1) !env in
    let closure = OV.Closure (arg, body, new_env) in
    new_env := Env.add name closure !env;
    interp scope new_env

  | Tt.AnonFuncCheck (arg_string, body) -> OV.Closure (arg_string, body, env)
  | Tt.AppCheck (func, param) ->
    begin match interp func env with
    | OV.Closure (name, body_tree, capture_env) ->
      let param_val = interp param env in
      let new_env = ref @@ Env.add name param_val !capture_env in
      interp body_tree new_env
    | _ -> raise InternalTCErr
    end
  | Tt.IfCheck (cond, tb, fb) ->
    begin match interp cond env with
    | OV.Bool true -> interp tb env
    | OV.Bool false -> interp fb env
    | _ -> raise InternalTCErr
    end
