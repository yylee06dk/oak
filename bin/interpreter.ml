module Op = Operator
module Tt = TypedTree

exception InternalTCErr

(* Uses locally abstract types to deal with each branch resulting in different types *)
let rec interp : type a. a Tt.t -> a = function
  | Tt.IntLiteral n -> n
  | Tt.FloatLiteral f -> f
  | Tt.BoolLiteral b -> b
  | Tt.BinOpInt (l,op,r) -> 
    begin match op with
    | Op.Plus  -> (interp l) + (interp r)
    | Op.Minus -> (interp l) - (interp r)
    | Op.Mult  -> (interp l) * (interp r)
    | Op.Div   -> (interp l) / (interp r)
    | _ -> raise InternalTCErr
    end
  | Tt.BinOpFloat (l,op,r) -> 
    begin match op with
    | Op.Plus  -> (interp l) +. (interp r)
    | Op.Minus -> (interp l) -. (interp r)
    | Op.Mult  -> (interp l) *. (interp r)
    | Op.Div   -> (interp l) /. (interp r)
    | _ -> raise InternalTCErr
    end
  | Tt.LogicOpBool (l,op,r) ->
    begin match op with
    | Op.And -> interp l && interp r
    | Op.Or  -> interp l || interp r
    | _ -> raise InternalTCErr 
    end 

