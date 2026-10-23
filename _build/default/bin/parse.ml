open Angstrom
module Utt = UntypedTree

let digits =
  take_while1 (function '0' .. '9' -> true | _ -> false)

let spaces =
  skip_while (function ' ' | '\n' | '\t' | '\r' -> true | _ -> false)

let parse_int =
  digits >>| fun s -> Utt.Valuen (int_of_string s)

let parse_float =
  consumed (digits *> char '.' *> digits)
  >>| fun s -> Utt.Valuef (float_of_string s)

let parse_literal =
  parse_float <|> parse_int

let parse_term =
  lift2 (fun first rest ->
      List.fold_left (fun tree right -> Utt.Term (tree, right)) first rest
    )
    parse_literal
    (many (spaces *> char '+' *> spaces *> parse_literal))

let parse_expr =
  spaces *> parse_term <* spaces
