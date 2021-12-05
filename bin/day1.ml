open Lib

let solve params lines =
  let window_size = params.(1) |> int_of_string in
  match
    lines |> Seq.map int_of_string |> Seq.slide_n window_size |> Seq.uncons
  with
  | None               -> failwith "no input"
  | Some (first, rest) ->
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
