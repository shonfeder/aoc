module type Solver = sig
  val solve : string Array.t -> string Zlist.t -> int
end

module Zlist = struct
  include Zlist

  let uncons z =
    let open Containers.Option.Infix in
    let+ x = Zlist.head z in
    (x, Zlist.tail z)

  let to_seq l = Seq.unfold uncons l

  let to_list z = Zlist.fold_left (Fun.flip List.cons) [] z |> List.rev

  let head_exn z =
    Zlist.head z |> Containers.Option.get_exn_or "head of empty Zlist.t"

  let mapi : (int -> 'a -> 'b) -> 'a t -> 'b t =
   fun f z ->
    let open Containers.Option.Infix in
    Zlist.unfold (0, z) (fun (i, z) ->
        let+ x, xs = uncons z in
        ((succ i, xs), f i x))
end

open Containers

(* TODO rm? *)
(* First see if anything can be upstreamed to zlist *)
module LSeq = struct
  type 'a t = 'a cell Lazy.t

  and 'a cell =
    | Cons of ('a * 'a t)
    | Nil

  let nil : 'a t = lazy Nil

  let cons x xs : 'a t = lazy (Cons (x, Lazy.from_fun xs))

  let uncons xs =
    match xs with
    | (lazy (Cons (x, xs))) -> Some (x, xs)
    | (lazy Nil)            -> None

  let rec unfold : ('src -> ('a * 'src) option) -> 'src -> 'a t =
   fun f src ->
    match f src with
    | Some (x, src) -> cons x (fun () -> Lazy.force (unfold f src))
    | None          -> nil

  let rec map : ('a -> 'b) -> 'a t -> 'b t =
   fun f l ->
    Lazy.from_fun (fun () ->
        Lazy.force
        @@
        match l with
        | (lazy (Cons (x, xs))) -> cons (f x) (fun () -> Lazy.force (map f xs))
        | (lazy Nil)            -> nil)

  let rec fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc =
   fun f init l ->
    match uncons l with
    | Some (x, xs) -> fold_left f (f init x) xs
    | None         -> init

  let lines_of_in_channel ic =
    let read () =
      try Some (input_line ic, ()) with
      | End_of_file -> None
    in
    unfold read ()

  let to_list t =
    let rec aux acc s =
      match uncons s with
      | None         -> List.rev acc
      | Some (x, xs) -> aux (x :: acc) xs
    in
    aux [] t
end

module IO = struct
  include IO

  let fold_lines_in f init ic =
    let rec aux acc =
      match input_line ic with
      | line                  -> aux (f acc line)
      | exception End_of_file -> acc
    in
    aux init

  let zlist_of_lines_in ic =
    let read_line () =
      try Some ((), input_line ic) with
      | End_of_file -> None
    in
    Zlist.unfold () read_line
end

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
  include List

  let sum ls = List.fold_left ( + ) 0 ls

  let remove_map f assoc =
    let rec aux ls acc =
      match ls with
      | []        -> (None, List.rev acc)
      | x :: rest ->
      match f x with
      | None   -> aux rest (x :: acc)
      | Some a -> (Some a, List.rev acc @ rest)
    in
    aux assoc []

  let map_in_channel_lines f ic =
    IO.fold_lines_in (fun acc x -> f x :: acc) [] ic |> List.rev
end

module String = struct
  include String

  let explode str =
    let len = length str in
    let rec aux : int -> string List.t -> string List.t =
     fun i acc ->
      if Int.(i < len) then
        aux (succ i) (String.sub str i 1 :: acc)
      else
        List.rev acc
    in
    aux 0 []
end

module Matrix = struct
  type 'a t = 'a Array.t Array.t

  let fold f init t =
    Array.fold
      (fun acc row -> Array.fold (fun acc' a -> f acc' a) acc row)
      init
      t

  let iter f t = fold (fun () x -> f x) () t

  open Option.Infix

  let set ~x ~y m v = m.(y).(x) <- v

  let get_opt ~x ~y t =
    let* row = Array.get_safe t y in
    Array.get_safe row x

  (* TODO it'd be cool to have slices over an underlying array *)

  (** A 3x3 matrix of optional values that are adjacent to position x,y
      note that position 1,1 in the array is always [None]  *)
  let adjacent ?(diagonal = false) ~x ~y t =
    let iter = Option.iter in
    let adj = Array.make_matrix 3 3 None in
    let set ~x ~y v = set ~x ~y adj (Some v) in
    let () =
      (* up *)
      get_opt ~x ~y:(y - 1) t |> iter (set ~x:1 ~y:0);
      (* down *)
      get_opt ~x ~y:(y + 1) t |> iter (set ~x:1 ~y:2);
      (* left *)
      get_opt ~x:(x - 1) ~y t |> iter (set ~x:0 ~y:1);
      (* right *)
      get_opt ~x:(x + 1) ~y t |> iter (set ~x:2 ~y:1);
      if diagonal then (
        (* up-left *)
        get_opt ~x:(x - 1) ~y:(y - 1) t |> iter (set ~x:0 ~y:0);
        (* up-right *)
        get_opt ~x:(x + 1) ~y:(y - 1) t |> iter (set ~x:2 ~y:0);
        (* down-left *)
        get_opt ~x:(x - 1) ~y:(y + 1) t |> iter (set ~x:0 ~y:2);
        (* down-right *)
        get_opt ~x:(x + 1) ~y:(y + 1) t |> iter (set ~x:2 ~y:2)
      )
    in
    adj

  (** A list of the elements in the matrix *)
  let elements t = fold (fun ls x -> x :: ls) [] t |> List.rev

  let map_in_channel_lines f ic =
    List.map_in_channel_lines f ic |> Array.of_list
end

let bin_digits_to_int : int list -> int =
 fun digits -> List.fold_left (fun acc d -> (acc * 2) + d) 0 digits
