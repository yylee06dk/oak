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
  let ident_start = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let ident_continue = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true |_ -> false in

  consumed (
    satisfy ident_start *> skip_while ident_continue
  ) >>= fun name ->
    match name with
    | "let" | "fun" | "in" | "true" | "false" | "and" | "or" | "if" | "then" | "else"
    -> fail ""
    | _ -> return @@ Utt.Identifier name

let parse_literal =
  trace "literal" @@ parse_float <|> parse_int <|> parse_bool <|> parse_ident <|> parse_string <|> parse_char

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
  
let arg_pair =
  lift2 Pair.make
  (spaces *> char '(' *> parse_ident <* spaces)
  (char ':' *> spaces *> parse_type <* spaces <* char ')')

let term_operator =
  spaces *> choice 
    [ char '+' *> return Op.Plus;
      char '-' *> return Op.Minus; ]

let factor_operator =
  spaces *> choice 
    [ char '*' *> return Op.Mult;
      char '/' *> return Op.Div; ]

let equality_operator =
  spaces *> choice 
    [ string "==" *> return Op.Eq;
      string "!=" *> return Op.Neq; ]

let comparison_operator =
  spaces *> choice 
    [ string "<=" *> return Op.Leq;
      char '<' *> return Op.Less;
      string ">=" *> return Op.Geq;
      char '>' *> return Op.Great; ]
    
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
    let app =
      let build_appchain func params = 
        List.fold_left (fun f param -> Utt.App (f, param))
          func params
      in
      trace "app" @@
        lift2 (fun f params -> build_appchain f params)
        (spaces *> atom)
        (many1 @@ (spaces *> atom))
      <|> atom
    in
    let factor = 
      trace "factor" @@ chain_left app factor_operator
    in
    let term =
      trace "term" @@ chain_left factor term_operator
    in
    let logic_and =
      trace "and" @@ chain_left term (spaces *> string "and" *> return Op.And)
    in
    let logic_or =
      trace "or" @@ chain_left logic_and (spaces *> string "or" *> return Op.Or)
    in
    let comparison =
      trace "comp" @@
        lift3 (fun left op right -> Utt.BinOp (left, op, right))
        (spaces *> logic_or)
        comparison_operator
        (spaces *> logic_or)
      <|> spaces *> logic_or
    in
    let equality =
      trace "eq" @@
      lift3 (fun left op right -> Utt.BinOp (left, op, right))
        (spaces *> comparison)
        equality_operator
        (spaces *> comparison)
      <|> spaces *> comparison
    in
    let let_expr =
      trace "let_expr" @@ 
      lift4 (fun name type_note value scope -> Utt.VarDecl (name, type_note, value, scope)) 
        (string "let" *> spaces *> parse_ident <* spaces)
        (char ':' *> spaces *> parse_type <* spaces)
        (char '=' *> expr <* string "in")
        (expr)
      <|>
      (lift4 (fun name arg_name arg_typ dest_typ body scope -> Utt.RecFunc (name, (arg_name, arg_typ), dest_typ, body, scope))
        (string "let rec" *> spaces *> parse_ident)
        (spaces *> char '(' *> parse_ident)
        (spaces *> char ':' *> spaces *> parse_type <* spaces <* char ')')
        (spaces *> char ':' *> spaces *> parse_type)
        <*> (spaces *> char '=' *> expr)
        <*> (spaces *> string "in" *> expr))
    in
    let fun_expr =
      (* Chaining process - can it be abstracted? *)
      let build_func arg_chain body =
        let starter = arg_chain |> List.rev |> List.hd
        and leftover = arg_chain |> List.rev |> List.tl |> List.rev in
        
        List.fold_right (fun arg_pair func -> Utt.AnonFunc (arg_pair, func))
          leftover (Utt.AnonFunc (starter, body)) 
      in
      
      trace "func" @@
        lift2 build_func
        (string "fun" *> many1 arg_pair <* spaces)
        (string "->" *> expr)
    in
    let if_expr =
      trace "if" @@
        lift3 (fun cond t f -> Utt.If (cond, t, f))
        (string "if" *> spaces *> expr)
        (string "then" *> spaces *> expr)
        (string "else" *> spaces *> expr)
    in
    let entry_point = let_expr <|> fun_expr <|> if_expr <|> equality in
    spaces *> entry_point <* spaces)
