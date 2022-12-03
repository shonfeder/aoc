(**

   - Goal: Find all paths through cave system
   - Input: A map of a cave system, represented as lines of [<a>-<b>] pairs,
     indicating that caves [a] and [b] are connected.
   - Output: The number of distinct *paths* from the *start* to *end* of the cave sytem

   - *start*: Starting cave, all paths should originate here
   - *end*: Ending cave, all paths should terminate here
   - *small caves*: Indicated by caps, should be visited at most once
   - *large caves*: Indicated by lower case, may be visisted n times
     - So cycle detection is only active for small caves

     - Note: if any large caves were directly connected, then leaving them out
       of cycle detection could result in infinite length paths. however, there
       are no such connections on the given inputs, so I'm simply going to ommit
       dealing with the case. A solution for this would be to store node-keys
       as sets, and merge large caves into one node anytime a direct connection
       was detected.
*)

open! Containers
open Lib

module type Domain = sig
  type t
  (** Map of the cave system *)

  val of_lines : string Zlist.t -> t

  module Cave : sig
    type t

    val of_string : string -> t

    val equal : t -> t -> bool

    val is_small : t -> bool
  end

  val cave_is_visited : t -> Cave.t -> bool

  val cave_visits : t -> Int.Set.t

  val note_visited : t -> Cave.t -> t

  val connected : t -> Cave.t -> Cave.t list

  module Path : sig
    type t = Cave.t list

    val empty : t

    val length : t -> int

    val add : Cave.t -> t -> t

    val to_string : t -> string
  end
end

module Solver (D : Domain) : Solver = struct
  open D

  let start_cave = Cave.of_string "start"

  let end_cave = Cave.of_string "end"

  (** Part 1 Algorithm:

     2. Begin with the start cave
     1. Initialize [path] to empty
     3. [explore] the current [cave]
     4. if current [cave = end], then add it to the [path], and return a singleton of [[path]]
     5. if [is_visited cave], then return []
     6. else, add [cave] to [path], and if, [is_small cave] mark current cave as [visited]
     7. and find all [connected cave], and fold [explore] over these,
     8. accumulating the paths, and filtering just the [Some] values
     9. Finally, count all paths discovered *)
  let find_all_paths can_visit (map : t) : int =
    let rec explore m paths cave =
      let find_paths m path c : Path.t list =
        if not (can_visit m c) then
          []
        else
          let paths' =
            let path = Path.add c path in
            if Cave.equal c end_cave then
              [ path ]
            else
              let m' =
                if Cave.is_small c then
                  note_visited m c
                else
                  m
              in
              connected map c
              |> List.fold_left (explore m') []
              |> List.map (List.append path)
          in
          paths'
      in
      paths @ find_paths m Path.empty cave
    in
    let paths = explore map [] start_cave in
    List.iter
      (fun p -> Logs.debug (fun f -> f "path: %s" (Path.to_string p)))
      paths;
    paths
    |> List.fold_filter_map (fun count path -> (succ count, Some path)) 0
    |> fst

  let visit_small_twice m c =
    if Fun.negate Cave.is_small c then
      true
    else if Cave.equal c start_cave then
      not (cave_is_visited m c)
    else
      (not (cave_is_visited m c)) || Fun.negate (Int.Set.mem 2) (cave_visits m)

  let solve params lines =
    let can_visit =
      match params.(1) with
      | "visit-small-once"  -> fun m c -> not (cave_is_visited m c)
      | "visit-small-twice" -> visit_small_twice
      | _                   -> failwith "invalid command to day12"
    in
    of_lines lines |> find_all_paths can_visit
end

module D : Domain = struct
  module Cave = struct
    type t = String.t

    let of_string = Fun.id

    let compare = String.compare

    let equal = String.equal

    module Set = Set.Make (String)

    let is_small cave =
      let c = String.get cave 0 in
      Char.equal c (Char.lowercase_ascii c)
  end

  module Path = struct
    include List

    type t = Cave.t list

    let add x path =
      Logs.debug (fun f -> f "Adding %s to path" x);
      cons x path

    let to_string t = String.concat "," t
  end

  module CaveMap = Map.Make (Cave)

  (* The cave system is represented as an adjacency map *)
  type t =
    { visited : int CaveMap.t
    ; graph : Cave.Set.t CaveMap.t
    }

  let of_lines =
    let parse str =
      match String.split ~by:"-" str with
      | [ a; b ] -> (a, b)
      | _        -> failwith ("Invalid line " ^ str)
    in
    let add_cave c = function
      | None       -> Some (Cave.Set.singleton c)
      | Some caves -> Some (Cave.Set.add c caves)
    in
    fun lines ->
      let graph =
        lines
        |> Zlist.fold_left
             (fun graph line ->
               let a, b = parse line in
               graph
               |> CaveMap.update a (add_cave b)
               |> CaveMap.update b (add_cave a))
             CaveMap.empty
      in
      { graph; visited = CaveMap.empty }

  let cave_visits t =
    t.visited |> CaveMap.to_list |> List.map snd |> Int.Set.of_list

  let cave_is_visited t cave =
    Logs.debug (fun f -> f "checking if visisted %s" cave);
    CaveMap.get cave t.visited |> Option.is_some

  let note_visited t cave =
    Logs.debug (fun f ->
        f
          "Noting %s as visited along with %s"
          cave
          (Path.to_string (CaveMap.to_list t.visited |> List.map fst)));
    let update_visits = function
      | None   -> Some 1
      | Some n -> Some (n + 1)
    in
    { t with visited = CaveMap.update cave update_visits t.visited }

  let connected t cave =
    let caves =
      CaveMap.get cave t.graph
      |> Option.get_exn_or ("unconnected cave " ^ cave)
      |> Cave.Set.elements
    in
    Logs.debug (fun f ->
        f "Caves connected to %s: %s" cave (String.concat "," caves));
    caves

  (* TODO Implementation of domain *)
end

(* Instantiate the solver *)
include Solver (D)
