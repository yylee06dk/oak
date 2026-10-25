open Angstrom
module Utt = UntypedTree
module Op = Operator

let debug_mode = true

(* Debugging facilities *)
let check_state label =
  if debug_mode then 
    pos >>= fun offset ->
    available >>= fun remaining ->
    peek_string (min 30 remaining) >>| fun upcoming ->
    Printf.eprintf "[%s] offset=%d upcoming=%S\n%!"
      label offset upcoming
  else return ()

let trace name p =
  check_state ("enter " ^ name) *>
  p <* check_state ("leave " ^ name)

let digits =
  take_while1 (function '0' .. '9' -> true | _ -> false)

let spaces =
  skip_while (function ' ' | '\n' | '\t' | '\r' -> true | _ -> false)

let parse_int =
  digits >>| fun s -> Utt.Literal (Utt.Box.Int (int_of_string s))

let parse_float =
  consumed (digits *> char '.' *> digits)
  >>| fun s -> Utt.Literal (Utt.Box.Float (float_of_string s))
  
let parse_bool =
  string "true" <|> string "false"
  >>| fun s -> Utt.Literal (Utt.Box.Bool (bool_of_string s))

let parse_literal =
  parse_float <|> parse_int <|> parse_bool
  
let term_operator =
  spaces *> choice 
    [ char '+' *> return Op.Plus;
      char '-' *> return Op.Minus;
      string "or" *> return Op.Or; ]

let factor_operator =
  spaces *> choice 
    [ char '*' *> return Op.Mult;
      char '/' *> return Op.Div;
      string "and" *> return Op.Or; ]
    
let chain_left operand operator =
  let chain =
    many @@ lift2 Pair.make operator (spaces *> operand)
  in
  let build =
    List.fold_left (fun left (op, right) -> Utt.BinOp (left, op, right)) 
  in
  operand >>= fun left ->
  chain >>= fun rights ->
  return @@ build left rights 
  
    
let parse_expr =
  fix (fun expr ->
    let group = char '(' *> expr <* char ')' in
    let atom = trace "atom" @@ group <|> parse_literal in
    let factor = 
      trace "factor" @@ chain_left atom factor_operator
    in
    let term =
      trace "term" @@ chain_left factor term_operator
    in
    spaces *> term <* spaces)
