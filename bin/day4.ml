module type Impl = sig
  type sq =
    { n : int
    ; marked : bool
    }

  module Board : sig
    type t = sq Array.t Array.t

    val print : t -> unit

    val mark_sq : int -> t -> (int * int) option * t

    val wins : int * int -> t -> bool

    val score : t -> int
  end

  type t =
    { numbers : int List.t
    ; boards : Board.t List.t
    }

  val draw_number : t -> (t * int) option

  val parse : string Zlist.t -> t option
end

module Solver (M : Impl) = struct
  let find_first_winner :
      ((int * int) option * M.Board.t) list -> (int, M.Board.t list) Either.t =
   fun boards ->
    boards
    |> List.fold_left
         (fun res (coords, board) ->
           match res with
           | Either.Left _       -> res
           | Either.Right boards ->
           match coords with
           | None        -> Either.right (board :: boards)
           | Some coords ->
               if M.Board.wins coords board then
                 Either.left (M.Board.score board)
               else
                 Either.right (board :: boards))
         (Either.right [])
    |> Either.map_right List.rev

  let solve params lines =
    let rec find_first st =
      match M.draw_number st with
      | None         -> failwith "no winner!"
      | Some (st, n) -> (
          let boards = List.map (M.Board.mark_sq n) st.boards in
          match find_first_winner boards with
          | Either.Left score   -> score * n
          | Either.Right boards -> find_first { st with boards })
    in
    let find_last (st : M.t) =
      st.numbers
      |> List.fold_left
           (fun ((st : M.t), prev_winner) n ->
             let coord_boards = List.map (M.Board.mark_sq n) st.boards in
             let winner, boards =
               List.partition_map
                 (fun (coords, board) ->
                   match coords with
                   | Some coords when M.Board.wins coords board ->
                       Either.left (Some (M.Board.score board * n))
                   | _ -> Either.right board)
                 coord_boards
             in
             let st' = { st with boards } in
             match (boards, winner) with
             | _, []     -> (st', prev_winner)
             | _, w :: _ -> (st', w)
             (* | _         -> failwith "Multiple winners" *))
           (st, None)
      |> snd
      |> Option.get
    in
    let st = M.parse lines |> Option.get in
    match params.(1) with
    | "first" -> find_first st
    | "last"  -> find_last st
    | _       -> failwith "Invalid command"
end

module Impl : Impl = struct
  open Lib

  type sq =
    { n : int
    ; marked : bool
    }

  module Board = struct
    type t = sq Array.t Array.t

    let print (t : t) =
      Array.iter
        (fun row ->
          Array.iter
            (fun sq ->
              Printf.printf
                "%2d-%s "
                sq.n
                (if sq.marked then
                  "x"
                else
                  "o"))
            row;
          Printf.printf "\n")
        t;
      print_newline ()

    let mapij f t = Array.mapi (fun i a -> Array.mapi (f i) a) t

    let mark_sq : int -> t -> (int * int) option * t =
     fun n t ->
      let coords = ref None in
      let t =
        t
        |> mapij (fun i j sq ->
               if Int.equal n sq.n then (
                 (* Printf.printf "Marked sq: %d at (%d,%d)\n" n i j; *)
                 coords := Some (i, j);
                 { sq with marked = true }
               ) else
                 sq)
      in
      (!coords, t)

    let row_wins n t =
      (* Printf.printf "checkig row: "; *)
      let res =
        t.(n)
        |> Array.fold_left
             (fun wins sq ->
               (* Printf.printf "%d-%b " sq.n sq.marked; *)
               sq.marked && wins)
             true
      in
      (* print_newline (); *)
      res

    let col_wins n t =
      (* Printf.printf "checkig col: "; *)
      let res =
        t
        |> Array.fold_left
             (fun wins row ->
               (* Printf.printf "%d-%b " row.(n).n row.(n).marked; *)
               row.(n).marked && wins)
             true
      in
      (* print_newline (); *)
      res

    let wins : int * int -> t -> bool =
     fun (i, j) t -> row_wins i t || col_wins j t

    let score : t -> int =
     fun t ->
      (* print_endline "SCORING"; *)
      t
      |> Array.map (fun row ->
             Array.to_list row
             |> List.filter_map (fun sq ->
                    if not sq.marked then
                      Some sq.n
                    else
                      None))
      |> Array.to_list
      |> List.concat
      |> List.sum
  end

  type t =
    { numbers : int List.t
    ; boards : Board.t List.t
    }

  let draw_number (t : t) =
    match t.numbers with
    | n :: numbers -> Some ({ t with numbers }, n)
    | []           -> None

  let whites = Str.regexp "[ \t]+"

  let parse lines =
    let open Option.Let in
    let* nums, rest = Zlist.uncons lines in
    let numbers = String.split_on_char ',' nums |> List.map int_of_string in
    let boards =
      Zlist.to_seq rest
      |> Seq.split_on (String.equal "")
      |> Seq.map Array.of_list
      |> Seq.map
           (Array.map (fun line ->
                line
                |> Str.split whites
                |> List.map int_of_string
                |> Array.of_list))
      |> Seq.map (Array.map (Array.map (fun n -> { n; marked = false })))
      |> Seq.to_list
    in
    (* List.iter Board.print boards; *)
    Some { numbers; boards }
end

include Solver (Impl)
