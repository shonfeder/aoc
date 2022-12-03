open! Containers
open Lib

module type Domain = sig
  type t
  (** A map of all the [octopus]' relative positions *)

  (* val print_map : t -> unit *)

  val of_lines : string Zlist.t -> t

  type octopus
  (** A dumbo octopus. *)

  val energy : octopus -> int
  (** Energy level of an octopus

      - Is a natural number
      - Cannot remain > 9 at the end of any [step] *)

  val has_flashed : octopus -> bool
  (** [has_flashed octopous] is [true] if [octopous] already flashed this step *)

  val increment_energy : octopus -> unit
  (** [increment_energy o] increments [o]'s energy by [1] *)

  val flash : t -> octopus -> unit
  (** [flash t octopus] flashes the [octopus] {b only} if [energy octopus > 9 &&
      not (has_flashed octopus)] otherwise, it is a no-op

      - when an octopous flashes, it increments the energy level of all [adjacent],
        and they may flash as well.
      - an octopus can only flash once per step  *)

  val reset : octopus -> unit
  (** [reset o] resets the energy of [o] if [energy o > 9] and sets [has_flashed
      o] to [false] *)

  val adjacent : octopus -> t -> octopus Option.t Array.t Array.t
  (** [adjacent octopus] is the list of octopodes that are above, below, next
      to, and at diagonals of, [octopus].

      Octopodes have at most 8 adjacent companions *)

  val fold : ('acc -> octopus -> 'acc) -> 'acc -> t -> 'acc

  val iter : (octopus -> unit) -> t -> unit
end

module Solver (D : Domain) : Solver = struct
  (** [step t] ticks the time, returns the number of flashes that ocurred in the step

      - All octopuses [increment_energy] (along with any entailed flashes)
      - After all increments and flashes, all energy levels > 9 are reset *)
  let step map : int =
    D.iter D.increment_energy map;
    D.iter (D.flash map) map;
    let flashes =
      D.fold (fun acc o -> acc + (D.has_flashed o |> Bool.to_int)) 0 map
    in
    D.iter D.reset map;
    flashes

  let flash octomap = Fun.iterate 100 (fun flash -> flash + step octomap) 0

  let sync octomap =
    let n = ref 1 in
    while step octomap < 100 do
      incr n
    done;
    !n

  let solve params lines =
    lines
    |> D.of_lines
    |>
    match params.(1) with
    | "flash" -> flash
    | "sync"  -> sync
    | _       -> failwith "day11: invalid command"
end

module D : Domain = struct
  type octopus =
    { x : int
    ; y : int
    ; mutable energy : int
    ; mutable flashed : bool
    }

  let _print { x; y; energy; flashed } =
    Printf.printf "(%d,%d): %d-%b; " x y energy flashed

  let of_string ~x ~y s =
    { x; y; energy = Int.of_string_exn s; flashed = false }

  let reset o =
    o.flashed <- false;
    if o.energy > 9 then o.energy <- 0

  let increment_energy o = o.energy <- succ o.energy

  let energy o = o.energy

  let has_flashed o = o.flashed

  type t = octopus Array.t Array.t

  let _print_map t =
    Array.iter
      (fun row ->
        Array.iter
          (fun o ->
            if o.energy < 10 then
              Printf.printf "%d" o.energy
            else
              Printf.printf "x")
          row;
        print_newline ())
      t;
    print_newline ()

  let of_lines lines =
    Zlist.mapi
      (fun y line ->
        String.explode line
        |> List.mapi (fun x o -> of_string ~x ~y o)
        |> Array.of_list)
      lines
    |> Zlist.to_list
    |> Array.of_list

  let adjacent { x; y; _ } t = Matrix.adjacent ~diagonal:true ~x ~y t

  let fold = Matrix.fold

  let iter = Matrix.iter

  let rec flash t o =
    if energy o <= 9 || has_flashed o then
      ()
    else (
      o.flashed <- true;
      let adj = adjacent o t in
      Matrix.iter (Option.iter increment_energy) adj;
      Matrix.iter (Option.iter (flash t)) adj
    )
end

(* Instantiate the solver *)
include Solver (D)
