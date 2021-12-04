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

  let map2 f s1 s2 =
    (s1, s2)
    |> Seq.unfold (fun (s1, s2) ->
           match (s1 (), s2 ()) with
           | Seq.Nil, Seq.Nil -> None
           | Seq.Cons (x1, s1'), Seq.Cons (x2, s2') -> Some (f x1 x2, (s1', s2'))
           | _ -> raise (Invalid_argument "Seq.map2 unequeal lists"))

  let to_list = List.of_seq

  let uncons s =
    match s () with
    | Seq.Nil          -> None
    | Seq.Cons (x, xs) -> Some (x, xs)

  let ( let* ) = Option.bind

  let ( let+ ) x f = Option.map f x

  let hd s =
    let+ x, _ = uncons s in
    x

  let tl s =
    let+ _, xs = uncons s in
    xs

  let partition f s = (Seq.filter f s, Seq.filter (Fun.negate f) s)
end

module List = struct
  include List

  let sum ls = List.fold_left ( + ) 0 ls
end

let bin_digits_to_int : int list -> int =
 fun digits -> List.fold_left (fun acc d -> (acc * 2) + d) 0 digits
