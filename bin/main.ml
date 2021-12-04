module type Solver = sig
  val solve : string Array.t -> int
end

let () =
  let (module Solver : Solver) =
    match Sys.getenv "AOC_DAY" with
    | "day1"              -> (module Day1)
    | "day2"              -> (module Day2)
    | "day3"              -> (module Day3)
    | _                   -> failwith "Invalid day"
    | exception Not_found ->
        print_endline "Missing AOC_DAY env var";
        exit 1
  in
  Solver.solve Sys.argv |> Printf.printf "%d"
