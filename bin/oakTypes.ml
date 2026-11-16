type t =
  | Int
  | Float
  | Bool
  | Char
  | String
  | Arrow of t * t

let rec pp fmt typ =
  (match typ with
  | Int -> Format.fprintf fmt "Int"
  | Float -> Format.fprintf fmt "Float"
  | Bool -> Format.fprintf fmt "Bool"
  | Char -> Format.fprintf fmt "Char"
  | String -> Format.fprintf fmt "String"
  | Arrow (src, dest) ->
    Format.fprintf fmt "@[<hov>";
    Format.fprintf fmt "%a@ ->@ %a"
      pp src pp dest);
    Format.fprintf fmt "@]"
      
let show typ =
  Format.asprintf "%a" pp typ