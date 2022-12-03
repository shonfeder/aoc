(**

   Part 1
- Goal: Determine the maximum sum of a sequences of sequences of ints
- Input: A list of groups of ints, one per line, with groups separated by empty lines
- Output: The value of the group with the highest sum
*)

open! Containers
open Lib

module type Domain = sig
  type elf_calories = int Zlist.t

  val max_snack_provider_calories : elf_calories -> int
  val top_3_snack_provider_calories : elf_calories -> int

  val parse : string Zlist.t -> elf_calories
end

module Solver (D : Domain) : Solver = struct

  let solve params lines =
    match params.(1) with
    | "1" -> lines |> D.parse |> D.max_snack_provider_calories
    | "2" -> lines |> D.parse |> D.top_3_snack_provider_calories
    | _ -> failwith "TODO Solution to puzzle"
end

module D : Domain = struct
  open Zlist
  open Fun.Infix

  type elf_calories = int Zlist.t

  let max_snack_provider_calories : elf_calories -> int =
    fun cals -> fold_left Int.max 0 cals

  let top_3_snack_provider_calories : elf_calories -> int =
    fun cals ->
    cals
    |> Zlist.to_list
    |> List.sort Int.compare
    |> List.rev
    |> List.take 3
    |> List.fold_left (+) 0

  let parse : string Zlist.t -> elf_calories =
    let take_callories lines =
      let cals =
        lines
        |> take_while (not % String.is_empty)
        |> map Int.of_string_exn
        |> fold_left (+) 0
      in
      let rest = drop_while (not % String.is_empty) lines |> tail
      in
      if cals > 0 then
        Some (rest, cals)
      else
        None
    in
    fun lines -> unfold lines take_callories
end

(* Instantiate the solver *)
include Solver (D)
