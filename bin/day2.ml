open Lib

type state =
  { horizontal : int
  ; depth : int
  ; aim : int
  }

let empty_state = { horizontal = 0; depth = 0; aim = 0 }

let _view_state ({ horizontal; depth; aim } as s) =
  Printf.printf "h: %d; d: %d; a: %d\n" horizontal depth aim;
  s

let update_state_part_1 s (dir, n) =
  match dir with
  | "forward" -> { s with horizontal = s.horizontal + n }
  | "up"      -> { s with depth = s.depth - n }
  | "down"    -> { s with depth = s.depth + n }
  | _         -> failwith ("Invalid direction " ^ dir)

let update_state_part_2 s (dir, n) =
  match dir with
  | "forward" ->
      { s with horizontal = s.horizontal + n; depth = s.depth + (s.aim * n) }
  | "up"      -> { s with aim = s.aim - n }
  | "down"    -> { s with aim = s.aim + n }
  | _         -> failwith ("Invalid direction " ^ dir)

let solve params lines =
  let parse_line ln =
    match String.split_on_char ' ' ln with
    | [ dir; n ] -> (dir, int_of_string n)
    | _          -> failwith ("invalid line: " ^ ln)
  in
  let update_f =
    match params.(1) with
    | "part1" -> update_state_part_1
    | "part2" -> update_state_part_2
    | _       -> failwith "invalid part"
  in
  lines
  |> Zlist.map parse_line
  |> Zlist.fold_left update_f empty_state
  |> fun { horizontal; depth; _ } -> horizontal * depth
