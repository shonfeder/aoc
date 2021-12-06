open Containers

module type Domain = sig
  (* TODO Specification of domain *)
  type t

  val of_string : string -> t

  val count_fish : t -> int

  (* Pass time by one unit (day) *)
  val tick : t -> unit
end

module Solver (D : Domain) = struct
  let solve : string Array.t -> string Seq.t -> int =
   fun params lines ->
    let days =
      params.(1)
      |> Int.of_string
      |> Option.get_exn_or "Invalid days: expected int"
    in
    let st = Seq.head_exn lines |> D.of_string in
    let () =
      for _ = 1 to days do
        D.tick st
      done
    in
    D.count_fish st
end

module D : Domain = struct
  type t = int Array.t

  let of_string s =
    let st = Array.make 9 0 in
    let () =
      s
      |> String.split_on_char ','
      |> List.iter (fun nstr ->
             let n = Int.of_string_exn nstr in
             st.(n) <- succ st.(n))
    in
    st

  let count_fish st = Array.fold_left ( + ) 0 st

  (* let show st =
   *   Printf.printf "Total fish %d" (count_fish st);
   *   print_string "Counters: ";
   *   Array.iter (Printf.printf "%d ") st;
   *   print_newline () *)

  let tick st =
    let expired_counters = st.(0) in
    for n = 1 to Array.length st - 1 do
      st.(n - 1) <- st.(n);
    done;
    st.(8) <- expired_counters;
    st.(6) <- st.(6) + expired_counters
end

(* Instantiate the solver *)
include Solver (D)
