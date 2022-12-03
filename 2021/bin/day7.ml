open Containers
module Zlist = Lib.Zlist

module type Domain = sig
  (* Crab positions *)
  type t

  val of_string : string -> t

  (* [move_to_pos t pos] gives fuels cost to move all crabs to position  *)
  val move_to_pos : fuel_rate:[ `Constant | `Variable ] -> t -> int -> int

  val positions : t -> int list

  val max_pos : t -> int

  val min_pos : t -> int
end

module Solver (D : Domain) = struct
  let solve : string Array.t -> string Zlist.t -> int =
   fun params lines ->
    let fuel_rate =
      match params.(1) with
      | "constant" -> `Constant
      | "variable" -> `Variable
      | _          -> failwith "Invalid fuel rate rate"
    in
    let move_to_pos = D.move_to_pos ~fuel_rate in
    let crabs = Zlist.head_exn lines |> D.of_string in
    let mean_pos =
      let positions = D.positions crabs in
      List.reduce_exn ( + ) positions / List.length positions
    in
    let mean_cost = move_to_pos crabs mean_pos in
    let plus_one = move_to_pos crabs (mean_pos + 1) in
    let minus_one = move_to_pos crabs (mean_pos - 1) in
    match
      if minus_one < mean_cost then
        Some (( + ) (-1))
      else if plus_one < mean_cost then
        Some (( + ) 1)
      else
        None
    with
    | None       -> mean_cost
    | Some delta ->
        let rec find_min_cost pos prev_cost =
          let next_pos = delta pos in
          let cost = move_to_pos crabs next_pos in
          if cost < prev_cost then
            find_min_cost next_pos cost
          else
            prev_cost
        in
        find_min_cost mean_pos mean_cost
end

module D : Domain = struct
  type t =
    { min : int
    ; max : int
    ; crabs : (int, int) Hashtbl.t
    }

  let of_string str =
    let crabs = Hashtbl.create 100 in
    String.split ~by:"," str
    |> List.map Int.of_string_exn
    |> List.iter (fun pos ->
           Hashtbl.update
             ~k:pos
             ~f:(fun _ n_crabs ->
               match n_crabs with
               | None   -> Some 1
               | Some n -> Some (succ n))
             crabs);
    let positions = Hashtbl.keys_list crabs in
    let min = List.reduce_exn min positions in
    let max = List.reduce_exn max positions in
    { crabs; min; max }

  let min_pos t = t.min

  let max_pos t = t.max

  let positions t = Hashtbl.keys_list t.crabs

  let constant_rate to_pos pos crabs acc = (abs (to_pos - pos) * crabs) + acc

  let triangle n =
    let rec aux acc m =
      if Int.equal m n then
        m + acc
      else
        aux (acc + m) (succ m)
    in
    aux 0 0

  let variable_rate to_pos pos crabs acc =
    (triangle (abs (to_pos - pos)) * crabs) + acc

  let move_to_pos ~fuel_rate t to_pos =
    let rate =
      match fuel_rate with
      | `Constant -> constant_rate to_pos
      | `Variable -> variable_rate to_pos
    in
    Hashtbl.fold rate t.crabs 0
end

(* Instantiate the solver *)
include Solver (D)
