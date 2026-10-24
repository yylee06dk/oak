module Op = Operator

type _ t =
  | IntLiteral : int -> int t 
  | FloatLiteral : float -> float t
  | BinOpInt : int t * Op.t * int t -> int t
  | BinOpFloat : float t * Op.t * float t -> float t

let rec pp : type a. Format.formatter -> a t -> unit =

  fun fmt tree -> let open Format in
    match tree with
    | IntLiteral n -> fprintf fmt "(IntLiteral %d)" n
    | FloatLiteral f -> fprintf fmt "(FloatLiteral %g)" f
    | BinOpInt (l, op, r) ->
        fprintf fmt "@[<hov 2>(BinOpInt@ %a@ %a@ %a)@]"
          pp l Op.pp op pp r
    | BinOpFloat (l, op, r) ->
        fprintf fmt "@[<hov 2>(BinOpFloat@ %a@ %a@ %a)@]"
          pp l Op.pp op pp r

let show tree =
  Format.asprintf "%a" pp tree
