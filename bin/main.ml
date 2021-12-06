module type Solver = sig
  val solve : string Array.t -> string Seq.t -> int
end

let solvers : (module Solver) Array.t =
  [| (module Day1)
   ; (module Day2)
   ; (module Day3)
   ; (module Day4)
   ; (module Day5)
   ; (module Day6)
  |]

let () =
  match Sys.getenv "AOC_DAY" with
  | exception Not_found ->
      print_endline "Missing AOC_DAY env var";
      exit 1
  | day                 ->
  match Array.get solvers (int_of_string day - 1) with
  | (exception Invalid_argument _)
  | (exception Failure _) ->
      print_endline ("Invalid AOC_DAY day: " ^ day);
      exit 1
  | (module Solver) ->
      stdin
      |> Lib.lines_of_in_channel
      |> Solver.solve Sys.argv
      |> Printf.printf "%d"
