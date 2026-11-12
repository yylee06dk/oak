open Angstrom
module Utt = UntypedTree
module Op = Operator
module OV = OakValues
module OT = OakTypes

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
  digits >>| fun s -> Utt.Literal (OV.Int (int_of_string s))

let parse_float =
  consumed (digits *> char '.' *> digits)
  >>| fun s -> Utt.Literal (OV.Float (float_of_string s))
  
let parse_bool =
  string "true" <|> string "false"
  >>| fun s -> Utt.Literal (OV.Bool (bool_of_string s))
  
let parse_char =
  char '\'' *> any_char <* char '\''
  >>| fun c -> Utt.Literal (OV.Char c)
  
let parse_string =
  char '\"' *>  
  consumed (skip_while (fun c -> c != '\"'))
  <* char '\"'
  >>| fun s -> Utt.Literal (OV.String s)

let parse_ident =
  consumed (
    any_char *> skip_while (function '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  ) >>| fun s -> Utt.Identifier s

let parse_literal =
  parse_float <|> parse_int <|> parse_bool <|> parse_ident <|> parse_string <|> parse_char
  
let parse_singleton_type =
  spaces *> choice
    [ string "Int" *> return OakTypes.Int;
      string "Float" *> return OakTypes.Float;
      string "Bool" *> return OakTypes.Bool;
      string "Char" *> return OakTypes.Char;
      string "String" *> return OakTypes.String; ]
  
let parse_type =
  fix (fun typ ->
    let group_type =
      char '(' *> spaces *> typ <* spaces <* char ')'
    in
    let atom =
      group_type <|> parse_singleton_type
    in
    let arrow_type =
      lift2 (fun l r -> OT.Arrow (l, r))
      (atom <* spaces <* string "->" <* spaces)
      typ
      <|> atom
    in
    spaces *> arrow_type <* spaces
  )
  
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
    let var_decl =
      trace "var_decl" @@ 
      lift4 (fun name type_note value scope -> Utt.VarDecl (name, type_note, value, scope)) 
        (trace "var_name" @@ string "let" *> spaces *> parse_ident <* spaces)
        (trace "var_init" @@ char ':' *> spaces *> parse_type <* spaces)
        (trace "var_scope" @@ char '=' *> expr <* string "in")
        expr
    in
    let atom = trace "atom" @@ var_decl <|> group <|> parse_literal in
    let factor = 
      trace "factor" @@ chain_left atom factor_operator
    in
    let term =
      trace "term" @@ chain_left factor term_operator
    in
    let func =
      let single_arg =
        lift2 Pair.make
        (spaces *> char '(' *> parse_ident <* spaces)
        (char ':' *> spaces *> parse_type <* spaces <* char ')')
      in
      trace "func" @@
        lift2 (fun arg_list body -> Utt.AnonFunc (arg_list, body))
        (string "fun" *> many single_arg <* spaces)
        (string "->" *> expr)
      <|> term
    in
    spaces *> func <* spaces)
