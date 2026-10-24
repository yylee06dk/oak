open Angstrom
module Utt = UntypedTree
module Op = Operator


let digits =
  take_while1 (function '0' .. '9' -> true | _ -> false)

let spaces =
  skip_while (function ' ' | '\n' | '\t' | '\r' -> true | _ -> false)

let parse_int =
  digits >>| fun s -> Utt.Literal (Utt.Box.Int (int_of_string s))

let parse_float =
  consumed (digits *> char '.' *> digits)
  >>| fun s -> Utt.Literal (Utt.Box.Float (float_of_string s))

let parse_literal =
  parse_float <|> parse_int
  
let term_operator =
  spaces *> choice 
    [ char '+' *> return Op.Plus;
      char '-' *> return Op.Minus ]

let factor_operator =
  spaces *> choice 
    [ char '*' *> return Op.Mult;
      char '/' *> return Op.Div ]
    
let parse_expr =
  fix (fun expr ->
    let group = char '(' *> expr <* char ')' in
    let atom = group <|> parse_literal in
    let factor = 
      lift2 
        (fun first rest ->
        List.fold_left (fun tree (op, right) -> Utt.BinOp (tree, op, right)) first rest)
        atom (* first *)
        (many ( (* rest *)
          Pair.make <$>
          factor_operator <*>
          (spaces *> atom)))
    in
    let term =
      lift2 
        (* If this turns out to just be an atom, folding will happen 0 times and naturally get there
           If it has more than one trailing [+ ATOM], it will be parsed then folded for left-recursiveness*)
        (fun first rest ->
        List.fold_left (fun tree (op, right) -> Utt.BinOp (tree, op, right)) first rest)
        factor (* starting point - first *)
        (many ( (* parse every optional [+ ATOM] to a tuple list - rest *)
          (fun x y -> (x,y)) <$>
          (spaces *> (string "+" <|> string "-") >>| Op.read) <*>
          (spaces *> factor)))
    in
    spaces *> term <* spaces)
