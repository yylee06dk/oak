module Op = Operator
module Env = struct
  include Map.Make(String)
  let pp pp_value fmt env =
    Format.fprintf fmt "Env:{@[<hov>";
    iter (fun key value ->
      Format.fprintf fmt "@ %s = %a;" key pp_value value)
      env;
    Format.fprintf fmt "@] }"
end


type t =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Closure of string * typed_tree * ((t Env.t) ref)

and typed_tree =
  | Literal of t
  | Identifier of string
  | BinOpInt of typed_tree * Op.t * typed_tree
  | BinOpFloat of typed_tree * Op.t * typed_tree
  | BinOpBool of typed_tree * Op.t * typed_tree
  | VarDeclCheck of string * typed_tree * typed_tree
  | RecFuncCheck of string * string * typed_tree * typed_tree
  | AnonFuncCheck of string * typed_tree
  | AppCheck of typed_tree * typed_tree
  | IfCheck of typed_tree * typed_tree * typed_tree
  [@@deriving show {with_path = false}]

  
let do_binOp op left right =
  match op with
  | Op.Plus -> (match left, right with
    | Int l, Int r -> Int (l + r)
    | Float l, Float r -> Float (l +. r)
    | _, _ -> failwith "InternalError")
  | Op.Minus -> (match left, right with
    | Int l, Int r -> Int (l - r)
    | Float l, Float r -> Float (l -. r)
    | _, _ -> failwith "InternalError")
  | Op.Mult -> (match left, right with
    | Int l, Int r -> Int (l * r)
    | Float l, Float r -> Float (l *. r)
    | _, _ -> failwith "InternalError")
  | Op.Div -> (match left, right with
    | Int l, Int r -> Int (l / r)
    | Float l, Float r -> Float (l /. r)
    | _, _ -> failwith "InternalError")
  | Op.And -> (match left, right with
    | Bool l, Bool r -> Bool (l && r)
    | _, _ -> failwith "InternalError")
  | Op.Or -> (match left, right with
    | Bool l, Bool r -> Bool (l || r)
    | _, _ -> failwith "InternalError")
  | Op.Eq -> (match left, right with
    | Int l, Int r -> Bool (l = r)
    | Float l, Float r -> Bool (l = r)
    | Bool l, Bool r -> Bool (l = r)
    | Char l, Char r -> Bool (l = r)
    | String l, String r -> Bool (l = r)
    | _, _ -> failwith "InternalError")
  | Op.Neq -> (match left, right with
    | Int l, Int r -> Bool (l <> r)
    | Float l, Float r -> Bool (l <> r)
    | Bool l, Bool r -> Bool (l <> r)
    | Char l, Char r -> Bool (l <> r)
    | String l, String r -> Bool (l <> r)
    | _, _ -> failwith "InternalError")
  | Op.Great -> (match left, right with
    | Int l, Int r -> Bool (l > r)
    | Float l, Float r -> Bool (l > r)
    | Char l, Char r -> Bool (l > r)
    | String l, String r -> Bool (l > r)
    | _, _ -> failwith "InternalError")
  | Op.Geq -> (match left, right with
    | Int l, Int r -> Bool (l >= r)
    | Float l, Float r -> Bool (l >= r)
    | Char l, Char r -> Bool (l >= r)
    | String l, String r -> Bool (l >= r)
    | _, _ -> failwith "InternalError")
  | Op.Less -> (match left, right with
    | Int l, Int r -> Bool (l < r)
    | Float l, Float r -> Bool (l < r)
    | Char l, Char r -> Bool (l < r)
    | String l, String r -> Bool (l < r)
    | _, _ -> failwith "InternalError")
  | Op.Leq -> (match left, right with
    | Int l, Int r -> Bool (l <= r)
    | Float l, Float r -> Bool (l <= r)
    | Char l, Char r -> Bool (l <= r)
    | String l, String r -> Bool (l <= r)
    | _, _ -> failwith "InternalError")
