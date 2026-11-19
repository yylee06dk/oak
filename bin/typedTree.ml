module Op = Operator
module OV = OakValues
module OT = OakTypes

type t = OakValues.typed_tree =
  | Literal of OV.t
  | Identifier of string
  | BinOpInt of t * Op.t * t
  | BinOpFloat of t * Op.t * t
  | BinOpBool of t * Op.t * t
  | VarDeclCheck of string * t * t
  | RecFuncCheck of string * string * t * t
  | AnonFuncCheck of string * t
  | AppCheck of t * t
  | IfCheck of t * t * t

let rec pp fmt tree =
  match tree with
  | Literal v -> Format.fprintf fmt "%a" OV.pp v
  | Identifier s -> Format.fprintf fmt "%s" s
  | BinOpInt (l, op, r)
  | BinOpFloat (l, op, r)
  | BinOpBool (l, op, r) ->
    Format.fprintf fmt "@[<hov 2>Bin(%a@ %a@ %a)@]"
      pp l Op.pp op pp r
  | VarDeclCheck (name, init, scope) ->
    Format.fprintf fmt "@[<hov 2>Var(%s =@ %a@ in@ %a)@]"
      name pp init pp scope
  | RecFuncCheck (name, arg_name, body, scope) ->
    Format.fprintf fmt "@[<hov 2>RecFun(%s: %s ->@ %a@ in@ %a)@]"
      name arg_name pp body pp scope
  | AnonFuncCheck (arg, body) ->
    Format.fprintf fmt "@[<hov 2>Fun(%s ->@ %a)@]"
      arg pp body
  | AppCheck (func, param) ->
    Format.fprintf fmt "@[<hov 2>App(%a@ %a)@]"
      pp func pp param
  | IfCheck (cond, tb, fb) ->
    Format.fprintf fmt "@[<hov 2>If(%a)@ then (%a)@ else (%a)@]"
      pp cond pp tb pp fb

let show tree =
  Format.asprintf "%a" pp tree
