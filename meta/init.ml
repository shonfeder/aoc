open Containers

let ml_skeleton =
  {|(**

- Goal: TODO
- Input: TODO
- Output: TODO
*)

open! Containers
open Lib

module Types = struct
  (* TODO Specification of domain *)

  type t = ..
end


module type Operations = sig
  include module type of Types

  val parse : string Zlist.t -> t
end

module Solver (O : Operations) : Solver = struct
  let solve params _lines = match params.(1) with
    | "1" -> failwith "TODO Solution to puzzle 1"
    | "2" -> failwith "TODO Solution to puzzle 2"
    |  c  -> failwith ("Invalid command " ^ c)
end

module O : Operations = struct
  include Types
  (* TODO Implementation of domain *)
end

(* Instantiate the solver *)
include Solver (O)
|}

let test_skeleton year day =
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
    year day day year year year year

let () =
  let year = Sys.getenv_opt "AOC_YEAR" |> Option.get_exn_or "Missing env var AOC_YEAR" in
  let day =
    1
    |> Array.get_safe Sys.argv |> Option.get_exn_or "Missing argument DAY"
    |> Int.of_string           |> Option.get_exn_or "Invalid argument, exepected int"
  in
  let ml_file_name = Printf.sprintf "./%s/bin/day%d.ml" year day in
  let test_dir_name = Printf.sprintf "./%s/test/day%d.t" year day in
  let test_run_name = Printf.sprintf "%s/run.t" test_dir_name in
  IO.File.write_exn ml_file_name ml_skeleton;
  Sys.mkdir test_dir_name 0o777;
  IO.File.write_exn test_run_name (test_skeleton year day)
