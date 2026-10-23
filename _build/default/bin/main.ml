open Parse
open Angstrom
module Utt = UntypedTree
module Tt = TypedTree

let tee f x = f x; x

let printUtt utt =
  print_endline "UTT:";
  print_endline (Utt.show_utt utt);
  print_newline ()
  
let printTt tt =
  print_endline "TT:";
  print_endline (Tt.show_tt tt);
  print_newline ()

let rec repl () =
  Printf.printf ">> %!";
  match read_line () with
  | input -> 
    let () =
      match parse_string ~consume:All parse_expr input with
      | Result.Ok utt ->
        utt |> tee printUtt
        |> MyTypeCheck.typeCheck
        |> fst |> tee printTt 
        |> Interpreter.interp 
        |> Printf.(function Boxn n -> printf "%d\n" n | Boxf f -> printf "%f\n" f)
      | Result.Error str -> print_endline str
    in
    repl ()
  | exception End_of_file -> print_endline "Terminating..."

let () = repl ()
