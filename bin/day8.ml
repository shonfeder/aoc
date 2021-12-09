open Containers
module List = Lib.List

module type Domain = sig
  (* A representation of an entry *)

  module Digit : sig
    type t

    val of_string : string -> t
  end

  type t

  val of_string : string -> t

  val output_digits : t -> int list

  val output_value : t -> int
end

module Solver (D : Domain) = struct
  let is_digit_with_unique_num_segments = function
    | 1
    | 7
    | 4
    | 8 ->
        true
    | _ -> false

  let count_unique_segment_num_digits acc entry =
    let digits =
      D.output_digits entry |> List.filter is_digit_with_unique_num_segments
    in
    List.length digits + acc

  let count_output_totals acc entry = acc + D.output_value entry

  let solve : string Array.t -> string Seq.t -> int =
   fun params lines ->
    let counter =
      match params.(1) with
      | "unique" -> count_unique_segment_num_digits
      | "totals" -> count_output_totals
      | cmd      -> failwith ("invalid command " ^ cmd)
    in
    Seq.map D.of_string lines
    |> Seq.fold_left counter 0
end

module D : Domain = struct
  module Digit = struct
    module CharSet = Set.Make (Char)
    include CharSet

    let of_string s = s |> String.to_seq |> of_seq

    let of_cardinality n t =
      if Int.equal n (cardinal t) then
        Some t
      else
        None
  end

  module SignalMap = Map.Make (Digit)

  type t =
    { signals : int SignalMap.t
    ; outputs : Digit.t list
    }

  exception Invalid_signal of string

  let determine_signals =
    let ( = ) = Int.equal in
    (* Quick and dirty little monad *)
    let put s () = (Some (), s) in
    let return a s = (a, s) in
    let ( let* ) o f s =
      match o s with
      | None, _       -> raise (Invalid_signal "None previous")
      | Some x, assoc -> f x assoc
    in
    fun signals ->
      let sig_map =
        let* () = put signals in
        let* one = List.remove_map (Digit.of_cardinality 2) in
        let* seven = List.remove_map (Digit.of_cardinality 3) in
        let* four = List.remove_map (Digit.of_cardinality 4) in
        let* eight = List.remove_map (Digit.of_cardinality 7) in
        let* nine =
          List.remove_map (fun d ->
              if Digit.cardinal d = 6 && Digit.subset four d then
                Some d
              else
                None)
        in
        let* zero =
          List.remove_map (fun d ->
              if Digit.cardinal d = 6 && Digit.subset seven d then
                Some d
              else
                None)
        in
        let* six =
          (* The only remaining 6 segment digit *)
          List.remove_map (fun d ->
              if Digit.cardinal d = 6 then
                Some d
              else
                None)
        in
        let* three =
          List.remove_map (fun d ->
              if Digit.cardinal d = 5 && Digit.subset one d then
                Some d
              else
                None)
        in
        let* five =
          List.remove_map (fun d ->
              if Digit.cardinal d = 5 && Digit.(equal d (inter nine six)) then
                Some d
              else
                None)
        in
        let* two =
          List.remove_map (fun d ->
              (* Only reminaing 5 segment digit *)
              if Digit.cardinal d = 5 then
                Some d
              else
                None)
        in
        return
          SignalMap.(
            empty
            |> add zero 0
            |> add one 1
            |> add two 2
            |> add three 3
            |> add four 4
            |> add five 5
            |> add six 6
            |> add seven 7
            |> add eight 8
            |> add nine 9)
      in
      sig_map () |> fst

  let of_string str =
    match String.split ~by:" | " str with
    | [ sigs; outs ] ->
        let signals =
          String.split ~by:" " sigs
          |> List.map Digit.of_string
          |> determine_signals
        in
        let outputs = String.split ~by:" " outs |> List.map Digit.of_string in
        { signals; outputs }
    | _              -> failwith ("Invalid entry: " ^ str)

  let decode_digit sig_map d =
    SignalMap.get d sig_map |> Option.get_exn_or "failed to decode digit"

  let output_digits t = t.outputs |> List.map (decode_digit t.signals)

  let output_value t =
    match output_digits t with
    | [ a; b; c; d ] -> (a * 1000) + (b * 100) + (c * 10) + d
    | _              -> failwith "Invalid bits for digit"
end

(* Instantiate the solver *)
include Solver (D)
