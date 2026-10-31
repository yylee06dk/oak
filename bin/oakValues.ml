module Op = Operator

type t =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  [@@deriving show]
  
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