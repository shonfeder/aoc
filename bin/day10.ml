open! Containers
open Lib

module type Domain = sig
  (* A line of chunks *)
  type t

  val of_string : string -> t

  val error_score : t -> int

  val completion_score : t -> int
end

module Solver (D : Domain) : Solver = struct
  let errors lines =
    lines |> Zlist.fold_left (fun score line -> score + D.error_score line) 0

  let completions lines =
    lines
    |> Zlist.filter (fun l -> not (D.error_score l > 0))
    |> Zlist.fold_left
         (fun score line ->
           List.sorted_insert ~cmp:Int.compare (D.completion_score line) score)
         []
    |> fun scores ->
    (* Getting the median *)
    List.nth scores (List.length scores / 2)

  let solve params lines =
    lines
    |> Zlist.map D.of_string
    |>
    match params.(1) with
    | "errors"      -> errors
    | "completions" -> completions
    | cmd           -> failwith ("Invalid command " ^ cmd)
end

module D : Domain = struct
  type t = Char.t List.t

  let of_string s = String.to_list s

  module CharSet = Set.Make (Char)

  let brackets = [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]

  let is_valid_pair open_bracket close_bracket =
    brackets
    |> List.exists
         (Pair.equal Char.equal Char.equal (open_bracket, close_bracket))

  let is_open c = List.exists (fun e -> fst e |> Char.equal c) brackets

  let is_close c = List.exists (fun e -> snd e |> Char.equal c) brackets

  (* Scoring corrupted lines *)
  let bracket_error_score = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _   -> failwith "Invalid character"

  let error_score t =
    let stack = Stack.create () in
    t
    |> List.fold_left
         (fun err bracket ->
           if Option.is_some err then
             err
           else if is_open bracket then
             let () = Stack.push bracket stack in
             None
           else
             let open Option.Infix in
             let* open_bracket = Stack.pop_opt stack in
             if is_valid_pair open_bracket bracket then
               None
             else
               Some (bracket_error_score bracket))
         None
    |> Option.value ~default:0

  let completion_score =
    (* We just check against the open bracket, cause we don't actually need to fetch its pair *)
    let points = function
      | '(' -> 1
      | '[' -> 2
      | '{' -> 3
      | '<' -> 4
      | _   -> failwith "Invalid bracket"
    in
    fun t ->
      let stack = Stack.create () in
      t
      |> List.rev
      |> List.fold_left
           (fun unpaired bracket ->
             if is_close bracket then
               let () = Stack.push bracket stack in
               unpaired
             else
               match Stack.pop_opt stack with
               | Some close_bracket when is_valid_pair bracket close_bracket ->
                   unpaired
               | None
               | Some _ ->
                   bracket :: unpaired)
           []
      |> List.rev
      |> List.fold_left (fun score c -> (score * 5) + points c) 0
end

(* Instantiate the solver *)
include Solver (D)
