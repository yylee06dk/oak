module Utt = UntypedTree
module Tt = TypedTree
module TC = TypeChecker

let tee f x = f x; x

(* Debugging functions *)
let printUtt utt =
  print_endline "UTT:";
  print_endline (Utt.show utt);
  print_newline ()
  
let printTt tt =
  print_endline "TT:";
  print_endline (Tt.show tt);
  print_newline ()
  
let run_line line =
  match Angstrom.(parse_string ~consume:All Parser.parse_expr line) with
  | Result.Ok utt ->
    begin match TC.typeCheck (tee printUtt utt) with
    | TC.IntType tree ->
      printTt tree;
      Printf.printf "%d\n" (Interpreter.interp tree)
    | TC.FloatType tree -> 
      printTt tree;
      Printf.printf "%g\n" (Interpreter.interp tree)
    | exception TC.TypeMismatch str ->
      print_endline str
    end
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
