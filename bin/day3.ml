open Lib

module Power = struct
  type bin_count =
    { zero : int
    ; one : int
    }

  type state = bin_count Seq.t

  let init digits : state =
    List.init digits (fun _ -> { zero = 0; one = 0 }) |> List.to_seq

  let count_bit count = function
    | 0 -> { count with zero = succ count.zero }
    | 1 -> { count with one = succ count.one }
    | n -> failwith ("invalid binary: " ^ string_of_int n)

  let gamma_of_state st =
    st
    |> Seq.map (fun s ->
           if s.one > s.zero then
             1
           else
             0)
    |> Seq.to_list
    |> bin_digits_to_int

  let epsilon_of_state st =
    st
    |> Seq.map (fun s ->
           if s.one < s.zero then
             1
           else
             0)
    |> Seq.to_list
    |> bin_digits_to_int

  let update_state : state -> int Array.t -> state =
   fun state line -> Seq.map2 count_bit state (Array.to_seq line)

  let solve (lines : int array Zlist.t) =
    let digits = Zlist.head lines |> Option.get |> Array.length in
    lines |> Zlist.fold_left update_state (init digits) |> fun st ->
    gamma_of_state st * epsilon_of_state st
end

let nth_bits : int -> int Array.t Seq.t -> int Array.t Seq.t =
 fun n lines -> Seq.map (fun l -> [| l.(n) |]) lines

module Life = struct
  type nums =
    { count : int
    ; vals : int Array.t List.t
    }

  type st =
    { column : int
    ; zeros : nums
    ; ones : nums
    }

  let init column =
    { column
    ; zeros = { count = 0; vals = [] }
    ; ones = { count = 0; vals = [] }
    }

  let add_line : st -> int Array.t -> st =
   fun st ln ->
    match ln.(st.column) with
    | 0 ->
        { st with
          zeros = { count = succ st.zeros.count; vals = ln :: st.zeros.vals }
        }
    | 1 ->
        { st with
          ones = { count = succ st.ones.count; vals = ln :: st.ones.vals }
        }
    | _ -> failwith "invalid bit"

  let next_col st = init (succ st.column)

  (* YUCK :( *)
  let solve (lines : int Array.t Zlist.t) =
    let process st lines = List.fold_left add_line st lines in
    let rec oxygen st lines =
      let st' = process st lines in
      let next = next_col st' in
      match (st'.ones.count, st'.zeros.count) with
      | 1, 1 -> st'.ones.vals |> List.hd
      | 0, 1 -> st'.zeros.vals |> List.hd
      | 1, 0 -> st'.ones.vals |> List.hd
      | o, z when o > z -> oxygen next st'.ones.vals
      | o, z when z > o -> oxygen next st'.zeros.vals
      | 0, 0 -> failwith "no nums"
      | o, z when o = z -> oxygen next st'.ones.vals
      | _ -> failwith ""
    in
    let rec scrubber st lines =
      let st' = process st lines in
      let next = next_col st' in
      match (st'.ones.count, st'.zeros.count) with
      | 1, 1 -> st'.zeros.vals |> List.hd
      | 0, 1 -> st'.zeros.vals |> List.hd
      | 1, 0 -> st'.ones.vals |> List.hd
      | o, z when o < z -> scrubber next st'.ones.vals
      | o, z when z < o -> scrubber next st'.zeros.vals
      | 0, 0 -> failwith "no nums"
      | o, z when o = z -> scrubber next st'.zeros.vals
      | _ -> failwith ""
    in
    let nums = Zlist.to_list lines in
    let st = init 0 in
    let ox = oxygen st nums |> Array.to_list |> bin_digits_to_int in
    let sc = scrubber st nums |> Array.to_list |> bin_digits_to_int in
    ox * sc
end

let solve params lines =
  let decoder =
    params.(1) |> function
    | "power" -> Power.solve
    | "life"  -> Life.solve
    | _       -> failwith "Invalid command"
  in
  lines
  |> Zlist.map (fun line ->
         String.to_seq line
         |> Array.of_seq
         |> Array.map (String.make 1)
         |> Array.map int_of_string)
  |> decoder
