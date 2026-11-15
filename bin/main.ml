module Utt = UntypedTree
module Tt = TypedTree
module TC = TypeChecker

let tee f x = f x; x

(* Debugging functions *)
let print_tree label show tree =
  Printf.printf "%s:\n%s\n\n" label (show tree)

let print_utt =
  print_tree "UTT" Utt.show

let print_tt tree =
  print_tree "TT" Tt.show tree

let run_line line =
  match Angstrom.(parse_string ~consume:All Parser.parse_expr line) with
  | Result.Ok utt ->
    TC.(match type_check (tee print_utt utt) TC.Context.empty with
    | Check (res_type, tree) ->
      print_endline (OT.show res_type);
      print_tt tree;
      Printf.printf "%s\n" (OakValues.show @@ Interpreter.interp tree Interpreter.Env.empty)
    | exception TC.TypeMismatch str ->
      print_endline str
    | exception Failure str ->
      print_endline str
    )
  | Result.Error str -> print_endline str


(* Where all the things come together! *)
let rec repl () =
  Printf.printf ">> %!";
  match read_line () with
  | line -> 
    run_line line;
    repl ()
  | exception End_of_file -> print_endline "Terminating..."

let () = repl ()
