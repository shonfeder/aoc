open Lib

let day1 params =
  let window_size = params.(1) |> int_of_string in
  match
    (lines_of_in_channel stdin
    |> Seq.map int_of_string
    |> Seq.slide_n window_size)
      ()
  with
  | Seq.Nil                -> failwith "no input"
  | Seq.Cons (first, rest) ->
      Seq.fold_left
        (fun (prev, count) window ->
          if List.length window < window_size then
            (prev, count)
          else
            let next = List.sum window in
            if prev < next then
              (next, succ count)
            else
              (next, count))
        (List.sum first, 0)
        rest
      |> snd
      |> Printf.printf "%d\n"

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

let day2 params =
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
  lines_of_in_channel stdin
  |> Seq.map parse_line
  |> Seq.fold_left update_f empty_state
  |> fun { horizontal; depth; _ } -> Printf.printf "%d\n" (horizontal * depth)

let solve = function
  | "day1" -> day1
  | "day2" -> day2
  | _      -> failwith "Invalid day"

let () =
  let day =
    try Sys.getenv "AOC_DAY" with
    | Not_found ->
        print_endline "Missing AOC_DAY env var";
        exit 1
  in
  solve day Sys.argv
