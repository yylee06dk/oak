module Op = Operator
module OV = OakValues
module OT = OakTypes

type t =
  | Literal of OV.t
  | Identifier of string
  | BinOp of t * Op.t * t
  | VarDecl of t * OT.t * t * t
  | RecFunc of t * (t * OT.t) * OT.t * t * t
  | AnonFunc of (t * OT.t) * t
  | App of t * t
  | If of t * t * t

let rec pp fmt tree =
  match tree with
  | Literal v -> Format.fprintf fmt "%a" OV.pp v
  | Identifier s -> Format.fprintf fmt "%s" s
  | BinOp (l, op, r) -> 
    Format.fprintf fmt "@[<hov 2>Bin(%a@ %a@ %a)@]"
      pp l Op.pp op pp r
  | VarDecl (name, typ, init, scope) ->
    Format.fprintf fmt "@[<hov 2>Var(%a: %a =@ %a@ in@ %a)@]"
      pp name OT.pp typ pp init pp scope
  | RecFunc (name, (arg, arg_typ), dest_typ, body, scope) ->
    Format.fprintf fmt "@[<hov 2>RecFun(%a@ (%a:%a):%a@ =@ %a in@ %a)@]"
      pp name pp arg OT.pp arg_typ OT.pp dest_typ pp body pp scope
  | AnonFunc ((arg, arg_typ), body) ->
    Format.fprintf fmt "@[<hov 2>Fun(%a: %a ->@ %a)@]"
      pp arg OT.pp arg_typ pp body
  | App (func, param) ->
    Format.fprintf fmt "@[<hov 2>App(%a@ %a)@]"
      pp func pp param
  | If (cond, tb, fb) ->
    Format.fprintf fmt "@[<hov 2>If(%a)@ then (%a)@ else (%a)@]"
      pp cond pp tb pp fb
  
let show tree =
  Format.asprintf "%a" pp tree
