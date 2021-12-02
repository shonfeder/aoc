let lines_of_in_channel ic =
  let read_line () =
    try Some (input_line ic, ()) with
    | End_of_file -> None
  in
  Seq.unfold read_line ()

module Seq = struct
  include Seq

  let take_n n seq =
    let rec aux count seq acc =
      if Int.equal count n then
        (List.rev acc, seq)
      else
        match seq () with
        | Seq.Nil            -> (List.rev acc, Seq.empty)
        | Seq.Cons (x, seq') -> aux (succ count) seq' (x :: acc)
    in
    aux 0 seq []

  let by_n : type a. int -> a Seq.t -> a list Seq.t =
   fun n seq ->
    seq
    |> Seq.unfold (fun seq ->
           match take_n n seq with
           | [], _    -> None
           | ls, seq' -> Some (ls, seq'))

  let drop_n : type a. int -> a Seq.t -> a Seq.t =
    fun n seq -> take_n n seq |> snd

  let slide_n : type a. int -> a Seq.t -> a list Seq.t =
   fun n seq ->
    if n < 1 then raise (Invalid_argument "slide_n with n =< 0");
    seq
    |> Seq.unfold (fun seq ->
           match take_n n seq with
           | [], _ -> None
           | ls, _ when List.length ls < n -> Some (ls, Seq.empty)
           | x :: xs, rest -> Some (x :: xs, Seq.append (List.to_seq xs) rest))
end

module List = struct
  include List

  let sum ls = List.fold_left ( + ) 0 ls
end
