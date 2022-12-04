open! Containers
open Lib

module Types = struct
  type item = char
  type rucksack = { a: item list ; b: item list }
  type safety_group = rucksack list
  type t = rucksack Zlist.t
end


module type Operations = sig
  include module type of Types

  val item_priority : item -> int
  val items : rucksack -> item list
  val safety_groups : t -> safety_group Zlist.t
  val group_badge_priority : safety_group -> int
  val common_items : rucksack -> item list
  val parse : string Zlist.t -> t
end

module Solver (O : Operations) : Solver = struct
  open Fun.Infix

  let solve_1 lines =
    let sum_priority_of_repeated_items =
      fun sum sack ->
        sum + (O.common_items sack |> List.map O.item_priority |> List.sum)
    in
    lines
    |> O.parse
    |> Zlist.fold_left sum_priority_of_repeated_items 0

  let solve_2 lines =
    lines
    |> O.parse
    |> O.safety_groups
    |> Zlist.map O.group_badge_priority
    |> Zlist.sum

  let solve params lines = match params.(1) with
    | "1" -> solve_1 lines
    | "2" -> solve_2 lines
    |  c  -> failwith ("Invalid command " ^ c)
end

module O : Operations = struct
  include Types
  module ItemSet = Set.Make (Char)

  let item_priority i = match i with
    | 'a'..'z' -> Char.code i - 96
    | 'A'..'Z' -> Char.code i - 38
    | _        -> failwith "Invalid item"

  let items {a; b} = a @ b
  let common_items {a; b} = ItemSet.(inter (of_list a) (of_list b) |> to_list)

  let safety_groups =
    let open Zlist in
    let group_of_three sacks =
      match take 3 sacks |>to_list with
      | [] -> None
      | group -> Some (drop 3 sacks, group)
    in
    fun sacks -> unfold sacks group_of_three

  let group_badge_priority : safety_group -> int =
    fun group ->
    group
    |> List.map (fun g ->
        items g
        |> (fun xs -> Logs.debug (fun f -> f "group items: %s" (String.of_list xs)); xs)
        |> ItemSet.of_list)
    |> List.reduce ItemSet.inter |> Option.get_exn_or "No common element!"
    |> ItemSet.to_list
    |> (fun xs -> Logs.debug (fun f -> f "common items: %s" (String.of_list xs)); xs)
    |> List.map item_priority
    |> List.sum

  let parse_sack str =
    let len = String.length str in
    let half = len / 2 in (* we're guarnateed even lengths *)
    let items = String.to_list str in
    { a = List.take half items
    ; b = List.drop half items
    }

  let parse lines = Zlist.map parse_sack lines
end

(* Instantiate the solver *)
include Solver (O)
