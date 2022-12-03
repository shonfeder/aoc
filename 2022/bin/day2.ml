(**

- Goal: TODO
- Input: TODO
- Output: TODO
*)

open! Containers
open Lib

module Types = struct
  (* TODO Specification of domain *)

  type shape = Rock | Paper | Scissors
  type outcome = Win | Draw | Lose
  type round = { oponent : shape ; self : shape option; outcome: outcome option}
  type t = round Zlist.t
end

module type Operations = sig
  include module type of Types

  val parse : string Zlist.t -> t
  val corrected_parse : string Zlist.t -> t
  val round_score : round -> int
end

module Solver (O : Operations) : Solver = struct
  let solve params lines = match params.(1) with
    | "1" -> lines |> O.parse |> Zlist.map O.round_score |> Zlist.sum
    | "2" -> lines |> O.corrected_parse |> Zlist.map O.round_score |> Zlist.sum
    |  c  -> failwith ("Invalid command " ^ c)
end

module O : Operations = struct
  open Fun.Infix
  include Types

  let oponent_shape = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "Invalid oponent_shape code"

  let my_shape = function
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith "Invalid my_shape code"

  (* Correct decoding *)
  let outcome = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "Invalid outcome code"

  let parse_round s =
    s
    |> String.split_on_char ' '
    |> function
    | [o; m] -> { oponent = oponent_shape o
                ; self = Some (my_shape m)
                ; outcome = None
                }
    | _ -> failwith "Invalid round"

  let corrected_parse_round s =
    s
    |> String.split_on_char ' '
    |> function
    | [o; m] -> { oponent = oponent_shape o
                ; self = None
                ; outcome = Some (outcome m)
                }
    | _ -> failwith "Invalid round"

  let shape_score = function
    | Rock     -> 1
    | Paper    -> 2
    | Scissors -> 3

  let round_outcome r = match r.self, r.oponent with
    | (Some Rock, Scissors) | (Some Scissors, Paper) | (Some Paper, Rock) -> Win
    | (Some Rock, Rock) | (Some Paper, Paper) | (Some Scissors, Scissors) -> Draw
    | _                                                                   -> Lose

  let round_shape r = match r.outcome, r.oponent with
    | (Some Win, Scissors) -> Rock
    | (Some Win, Paper) -> Scissors
    | (Some Win, Rock) -> Paper
    | (Some Lose, Scissors) -> Paper
    | (Some Lose, Paper) -> Rock
    | (Some Lose, Rock) -> Scissors
    | (Some Draw, shape) -> shape
    | None, _ -> failwith "Invalid corrected round"

  let outcome_score = function
    | Win  -> 6
    | Draw -> 3
    | Lose -> 0

  let round_score : round -> int =
    fun r ->
    let r = match r.outcome with
      | None -> {r with outcome = Some (round_outcome r)}
      | Some _ -> {r with self = Some (round_shape r)}
    in
    let shape = shape_score (Option.get_exn_or "missing self shape" r.self) in
    let outcome = outcome_score (Option.get_exn_or "missing outcome" r.outcome) in
    shape + outcome

  let parse lines = Zlist.map parse_round lines

  let corrected_parse lines = Zlist.map corrected_parse_round lines
end

(* Instantiate the solver *)
include Solver (O)
