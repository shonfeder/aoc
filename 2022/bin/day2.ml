(**

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
