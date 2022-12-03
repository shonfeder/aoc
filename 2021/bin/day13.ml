(**

- Goal: TODO
- Input: A list of x, y {b coordinates} and a list of {b fold instructions}



- {b coordinates}: The coordinates represent dots on transparent paper, using x,y coordinates
- {b fold instructions}: An instruction indictes a fold, which causes the grid to split
  at the indicated line, and one half inverted, and placed on top of the other.
   - [fold along y=n]: fold UP along the line running accros y coordinates [n]
   - [fold along x=n]: fold LEFT along the line running accross x coordinates [n]

- {b fold up}
   - split the grid {b on} the line accross all y coordinates (removing those coordinates)
   - reverse each column on the lower grid to "flip" the dots
   - combine the two grids, merging any overlapping dots
- {b fold left}
   - split the grid {b on} the line accross all x coordinates (removing those coordinates)
   - reverse each row on the right grid to "flip" the dots
   - combine the two grids, merging any overlapping dots

{{1} Part 1}

- Output: The number of distinct dots left after applying all foldings

- Plan:
  - implement sparse 2-d grid data structure (See Lib.Grid )
  - use operations to build then manipulate the grids

{{2} Part 1}
*)

open! Containers
open Lib

module type Domain = sig
  (* The paper *)
  type t

  (* A folding instruction *)
  type instruction = t -> t

  val fold_up : int -> instruction

  val fold_left : int -> instruction

  val count_dots : t -> int

  val parse : string Zlist.t -> t * instruction list

  val print : t -> unit
end

module Solver (D : Domain) : Solver = struct
  open D

  let solve params lines =
    let paper, instructions = parse lines in
    match params.(1) with
    | "first-fold" -> (List.hd instructions) paper |> count_dots
    | "code-word"  ->
        let result = List.fold_left (fun t fold -> fold t) paper instructions in
        print result;
        count_dots result
    | cmd          -> failwith ("Invalid command " ^ cmd)
end

module D : Domain = struct
  type t = unit Grid.t

  let print =
    let p = function
      | None    -> " "
      | Some () -> "#"
    in
    fun t ->
      let {x_axis; y_axis} : Grid.dimensions = Grid.dimensions t in
      let m = Grid.to_matrix t in
      for y = 0 to y_axis do
        for x = 0 to x_axis do
          print_string (p m.(x).(y))
        done;
        print_newline ()
      done
      (* |> Array.fold_left
       *      (fun str row ->
       *        let line = Array.map p row |> Array.to_list |> String.concat "" in
       *        str ^ line ^ "\n")
       *      "" *)

  (* let print t = to_string t |> print_endline *)

  (* (Grid.foldi (fun ~x ~y _ acc -> Printf.sprintf "(%d,%d)" x y :: acc) t []
   * |> List.rev
   * |> String.concat " ; ")
   * ^ "\n" *)

  type instruction = t -> t

  let fold_up y t =
    Logs.debug (fun f -> f "applying fold up y=%d" y);
    let top, bottom = Grid.split_on_y y t in
    let bottom' = Grid.flip_along_y bottom in
    let g = Grid.combine top bottom' in
    (* Logs.debug (fun f -> f "result: %s" (to_string g)); *)
    g

  let fold_left x t =
    Logs.debug (fun f -> f "applying fold left x=%d" x);
    let left, right = Grid.split_on_x x t in
    let right' = Grid.flip_along_x right in
    let g = Grid.combine left right' in
    (* Logs.debug (fun f -> f "result:\n%s" (to_string g)); *)
    g

  let count_dots = Grid.cardinal

  let parse lines =
    let line_is_empty = String.equal "" in
    let dots =
      lines
      |> Zlist.take_while (Fun.negate line_is_empty)
      |> Zlist.fold_left
           (fun acc l ->
             String.split_on_char ',' l |> fun l ->
             let x = List.nth l 0 |> Int.of_string_exn in
             let y = List.nth l 1 |> Int.of_string_exn in
             Grid.add ~x ~y () acc)
           Grid.empty
    in
    let instructions =
      lines
      |> Zlist.drop_while (Fun.negate line_is_empty)
      |> Zlist.tail
      |> Zlist.fold_left
           (fun instructions l ->
             let parts = String.split_on_char ' ' l in
             let inst =
               match
                 List.nth_opt parts 2
                 |> Option.get_exn_or "invalid instruction parts"
                 |> String.split ~by:"="
               with
               | [ "y"; n ] -> fold_up (Int.of_string_exn n)
               | [ "x"; n ] -> fold_left (Int.of_string_exn n)
               | _          -> failwith ("Invalid instruction " ^ l)
             in
             inst :: instructions)
           []
      |> List.rev
    in

    (dots, instructions)
end

(* Instantiate the solver *)
include Solver (D)
