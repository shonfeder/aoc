let lines_of_in_channel ic =
  let read_line () =
    try Some (input_line ic, ()) with
    | End_of_file -> None
  in
  Seq.unfold read_line ()

module Option = struct
  include Option

  module Let = struct
    let ( let* ) = bind

    let ( let+ ) x f = map f x
  end

  let some_if b thunk =
    if b then
      Some (thunk ())
    else
      None
end

module Seq = struct
  include Seq

  let uncons s =
    match s () with
    | Seq.Nil          -> None
    | Seq.Cons (x, xs) -> Some (x, xs)

  open Option.Let

  let hd s =
    let+ x, _ = uncons s in
    x

  let tl s =
    let+ _, xs = uncons s in
    xs

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

  let take_while : ('a -> bool) -> 'a Seq.t -> 'a list * 'a Seq.t =
   fun f seq ->
    let rec loop acc seq =
      match uncons seq with
      | None         -> (List.rev acc, seq)
      | Some (x, xs) ->
          if f x then
            loop (x :: acc) xs
          else
            (List.rev acc, cons x seq)
    in
    loop [] seq

  let drop_while : ('a -> bool) -> 'a Seq.t -> 'a Seq.t =
   fun f seq -> take_while f seq |> snd

  let split_on : ('a -> bool) -> 'a Seq.t -> 'a list Seq.t =
    (* takes xs for which `f x` is true, and drops empty sequences for which it is not *)
    let rec taker f seq =
      let taken, rest = take_while (Fun.negate f) seq in
      let rest = drop_while f rest in
      match taken with
      | _ :: _ -> Some (taken, rest)
      | []     ->
          let* x, xs = uncons rest in
          taker f (cons x xs)
    in
    fun f seq -> seq |> Seq.unfold (fun seq -> taker f seq)
end

module List = struct
  include Containers.List

  let sum ls = List.fold_left ( + ) 0 ls

  let remove_map f assoc =
    let rec aux ls acc =
    match ls with
    | [] -> (None, List.rev acc)
    | x :: rest -> match f x with
      | None -> aux rest (x :: acc)
      | Some a -> (Some a, List.rev acc @ rest)
    in
    aux assoc []
end

let bin_digits_to_int : int list -> int =
 fun digits -> List.fold_left (fun acc d -> (acc * 2) + d) 0 digits
