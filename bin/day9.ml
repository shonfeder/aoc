open Containers
open Lib

module type Domain = sig
  (* Map of positions *)
  type pos

  module PosSet : Set.S with type elt = pos

  val print : pos -> unit

  type t

  val of_lines : string Zlist.t -> t

  val fold : ('a -> pos -> 'a) -> 'a -> t -> 'a

  val adjacent : pos -> t -> pos list

  val height : pos -> int

  val mark_visited : pos -> unit

  val is_visited : pos -> bool
end

module Solver (D : Domain) = struct
  let is_lowest i ns = List.fold_left (fun b n -> b && i < n) true ns

  let calculate_risk n = n + 1

  let low_points map =
    map
    |> D.fold
         (fun acc pos ->
           let heights = D.adjacent pos map |> List.map D.height in
           if is_lowest (D.height pos) heights then
             acc + calculate_risk (D.height pos)
           else
             acc)
         0

  let basins map =
    let rec searcher basin pos =
      if D.height pos >= 9 then
        basin
      else
        let basin =
          if D.is_visited pos then
            basin
          else
            let () = D.mark_visited pos in
            D.PosSet.add pos basin
        in
        let neighbors =
          D.adjacent pos map
          |> List.filter (fun x -> D.height x < 9 && not (D.is_visited x))
        in
        D.PosSet.union basin (List.fold_left searcher D.PosSet.empty neighbors)
    in
    D.fold (fun acc pos -> searcher D.PosSet.empty pos :: acc) [] map
    |> List.map D.PosSet.cardinal
    |> List.sort Int.compare
    |> List.rev
    |> List.take 3
    |> List.reduce_exn ( * )

  let solve : string Array.t -> string Zlist.t -> int =
   fun params lines ->
    D.of_lines lines
    |>
    match params.(1) with
    | "lows"   -> low_points
    | "basins" -> basins
    | _        -> failwith "invalid command to day9"
end

module D : Domain = struct
  type pos =
    { x : int
    ; y : int
    ; height : int
    ; mutable visited : bool
    }

  module PosComp = struct
    type t = pos

    let compare a b = Pair.compare Int.compare Int.compare (a.x, a.y) (b.x, b.y)
  end

  module PosSet = Set.Make (PosComp)

  let print { x; y; height; visited } =
    Printf.printf "(%d,%d): %d;%b%!" x y height visited

  let mark_visited p = p.visited <- true

  let is_visited p = p.visited

  let height p = p.height

  type t = pos Array.t Array.t

  (* let print t =
   *   Array.iter (fun row -> Array.iter (Printf.printf "%d ") row; print_newline ()) t *)

  let of_lines lines =
    lines
    |> Zlist.mapi (fun y l ->
           l
           |> String.explode
           |> List.mapi (fun x n ->
                  { x; y; height = Int.of_string_exn n; visited = false })
           |> Array.of_list)
    |> Zlist.to_list
    |> Array.of_list

  open Option.Infix

  let adjacent { x; y; _ } (t : t) =
    List.filter_map
      Fun.id
      [ (let* up_row = Array.get_safe t (y + 1) in
         Array.get_safe up_row x)
      ; (let* down_row = Array.get_safe t (y - 1) in
         Array.get_safe down_row x)
      ; (let* row = Array.get_safe t y in
         Array.get_safe row (x + 1))
      ; (let* row = Array.get_safe t y in
         Array.get_safe row (x - 1))
      ]

  let fold f init t =
    Array.fold
      (fun acc row -> Array.fold (fun acc' a -> f acc' a) acc row)
      init
      t
end

(* Instantiate the solver *)
include Solver (D)
