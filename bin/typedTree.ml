type _ t =
  | IntLiteral : int -> int t 
  | FloatLiteral : float -> float t
  | AddInt : int t * int t -> int t
  | AddFloat : float t * float t -> float t

let rec pp : type a. Format.formatter -> a t -> unit =

  fun fmt tree -> let open Format in
    match tree with
    | IntLiteral n -> fprintf fmt "(IntLiteral %d)" n
    | FloatLiteral f -> fprintf fmt "(FloatLiteral %g)" f
    | AddInt (l, r) ->
        fprintf fmt "@[<hov2>(AddInt@ %a@ %a)@]" pp l pp r
    | AddFloat (l, r) ->
        fprintf fmt "@[<hov2>(AddFloat@ %a@ %a)@]" pp l pp r

let show tree =
  Format.asprintf "%a" pp tree
