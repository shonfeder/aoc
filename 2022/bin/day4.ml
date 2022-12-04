open! Containers
open Lib

module Types = struct
  (* TODO Specification of domain *)

  type section_range = int * int
  type assignment_pair = section_range * section_range
  type t = assignment_pair Zlist.t
end


module type Operations = sig
  include module type of Types

  val range_includes : section_range -> section_range -> bool
  val range_overlaps : section_range -> section_range -> bool
  val parse : string Zlist.t -> t
end

module Solver (O : Operations) : Solver = struct
  let solve_1 lines =
    lines
    |> O.parse
    |> Zlist.map (fun (a, b) -> if O.range_includes a b || O.range_includes b a then 1 else 0)
    |> Zlist.sum

  let solve_2 lines =
    lines
    |> O.parse
    |> Zlist.map (fun (a, b) -> if O.range_overlaps a b || O.range_overlaps b a then 1 else 0)
    |> Zlist.sum

  let solve params lines = match params.(1) with
    | "1" -> solve_1 lines
    | "2" -> solve_2 lines
    |  c  -> failwith ("Invalid command " ^ c)
end

module O : Operations = struct
  include Types

  let range_includes (a, b) (a', b') = a' >= a && b' <= b

  let range_overlaps (a, b) (a', b') = (a' >= a && a' <= b) || (b' <= b && b' >= a)

  let parse_range str : section_range =
    match String.split_on_char '-' str with
    | [a; b] -> (Int.of_string_exn a, Int.of_string_exn b)
    | _      -> failwith "Invalid section range"


  let parse_pair str : assignment_pair =
    match String.split_on_char ',' str with
    | [a; b] -> parse_range a, parse_range b
    | _      -> failwith "Invalid assignment pair"

  let parse lines : t = Zlist.map parse_pair lines
end

(* Instantiate the solver *)
include Solver (O)
