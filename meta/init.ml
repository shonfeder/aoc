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
  Printf.sprintf
    {|https://adventofcode.com/2021/day/%d

  $ export AOC_DAY=%d

Part 1:

  $ aoc2021 < test


  $ aoc2021 < input
|}
    day day

let () =
  let day =
    Array.get_safe Sys.argv 1
    |> Option.get_exn_or "Missing argument DAY"
    |> Int.of_string
    |> Option.get_exn_or "Invalid argument, exepected int"
  in
  let ml_file_name = Printf.sprintf "./bin/day%d.ml" day in
  let test_dir_name = Printf.sprintf "./test/day%d.t" day in
  let test_run_name = Printf.sprintf "%s/run.t" test_dir_name in
  IO.File.write_exn ml_file_name ml_skeleton;
  Sys.mkdir test_dir_name 0o777;
  IO.File.write_exn test_run_name (test_skeleton day)
