open Lib

let solve params =
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
