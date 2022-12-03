open Containers

let ml_skeleton =
  {|(**

- Goal: TODO
- Input: TODO
- Output: TODO
*)

open! Containers
open Lib

module type Domain = sig
  (* TODO Specification of domain *)
end

module Solver (D : Domain) : Solver = struct
  let solve _params _lines =
      failwith "TODO Solution to puzzle"
end

module D : Domain = struct
  (* TODO Implementation of domain *)
end

(* Instantiate the solver *)
include Solver (D)
|}

let test_skeleton day =
  let aoc_year = Sys.getenv_opt "AOC_YEAR" |> Option.get_exn_or "Missing env var AOC_YEAR" in
  Printf.sprintf
    {|https://adventofcode.com/%s/day/%d

  $ export AOC_DAY=%d

Part 1:

  $ aoc%s 1 < test


  $ aoc%s 1 < input

Part 1:

  $ aoc%s 2 < test


  $ aoc%s 2 < input
|}
    aoc_year day day aoc_year aoc_year aoc_year aoc_year

let () =
  let day =
    1
    |> Array.get_safe Sys.argv |> Option.get_exn_or "Missing argument DAY"
    |> Int.of_string           |> Option.get_exn_or "Invalid argument, exepected int"
  in
  let ml_file_name = Printf.sprintf "./bin/day%d.ml" day in
  let test_dir_name = Printf.sprintf "./test/day%d.t" day in
  let test_run_name = Printf.sprintf "%s/run.t" test_dir_name in
  IO.File.write_exn ml_file_name ml_skeleton;
  Sys.mkdir test_dir_name 0o777;
  IO.File.write_exn test_run_name (test_skeleton day)
