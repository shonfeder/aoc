open Containers

module Coord = struct
  type t = int * int

  let compare : t -> t -> int =
   fun a b -> Pair.compare Int.compare Int.compare a b
end

module type Domain = sig
  module Vent : sig
    type t =
      { start : int * int
      ; end_ : int * int
      }

    val is_diagonal : t -> bool

    val of_string : string -> t
  end

  module VentMap : module type of Map.Make (Coord)

  type map = int VentMap.t

  val add_vent_line : ?diagonal:bool -> map -> Vent.t -> map

  val count_overlapping_vents : map -> int

  val parse : string Zlist.t -> Vent.t Zlist.t
end

module Solver (M : Domain) = struct
  let solve params lines =
    let diagonal =
      match Array.get_safe params 1 with
      | None -> false
      | Some "diagonal" -> true
      | Some _ -> failwith "Invalid argument: only accept 'diagonal'"
    in
    let vents = M.parse lines in
    let map = Zlist.fold_left (M.add_vent_line ~diagonal) M.VentMap.empty vents in
    M.count_overlapping_vents map
end

module D : Domain = struct
  module Vent = struct
    type t =
      { start : int * int
      ; end_ : int * int
      }

    let is_diagonal { start = x1, y1; end_ = x2, y2 } = x1 <> x2 && y1 <> y2

    let arrow = Str.regexp " -> "

    let of_string s =
      let coord s =
        match String.split_on_char ',' s with
        | [ x; y ] -> (int_of_string x, int_of_string y)
        | _        -> failwith (Printf.sprintf "Invalid coordinate: %s" s)
      in
      match Str.split arrow s with
      | [ s; e ] -> { start = coord s; end_ = coord e }
      | _        -> failwith ("Invalid vent entry: " ^ s)

    let points : t -> Coord.t list =
     fun { start = x1, y1; end_ = x2, y2 } ->
      if Int.equal x1 x2 then
        List.(y1 -- y2) |> List.map (fun y -> (x1, y))
      else if Int.equal y1 y2 then
        List.(x1 -- x2) |> List.map (fun x -> (x, y1))
      else
        (* Diagonals -- only 45 degrees *)
        let xs = List.(x1 -- x2) in
        let ys = List.(y1 -- y2) in
        List.map2 (fun x y -> (x, y)) xs ys
  end

  module VentMap = Map.Make (Coord)

  type map = int VentMap.t

  let add_point map point =
    VentMap.update
      point
      (function
        | None   -> Some 1
        | Some n -> Some (succ n))
      map

  let add_vent_line ?(diagonal = false) m v =
    if (not diagonal) && Vent.is_diagonal v then
      m
    else
      Vent.points v |> List.fold_left add_point m

  let count_overlapping_vents m =
    VentMap.bindings m
    |> List.map snd
    |> List.fold_left (fun acc x -> if x > 1 then succ acc else acc ) 0

  let parse lines = lines |> Zlist.map Vent.of_string
end

include Solver (D)
