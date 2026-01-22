open Format
open Myutil

module List = struct
  include List

  let safe_hd : 'a list -> 'a option = function [] -> None | x :: _ -> Some x

  let safe_tl : 'a list -> 'a list option = function
    | [] -> None
    | _ :: tl -> Some tl

  let rec sep : 'a -> 'a list -> 'a list =
   fun s -> function
    | [] -> []
    | x :: [] -> [ x ]
    | x :: tl -> x :: s :: sep s tl

  let rec map_sep : 'b -> ('a -> 'b) -> 'a list -> 'b list =
   fun s f -> function
    | [] -> []
    | x :: [] -> [ f x ]
    | x :: tl -> f x :: s :: map_sep s f tl

  let uncons : 'a list -> ('a * 'a list) option = function
    | [] -> None
    | x :: tl -> Some (x, tl)

  let drop_tail : 'a list -> 'a list option =
   fun l -> (Option.map snd **. uncons **. rev) @@ l

  let make : int -> 'a -> 'a list =
   fun n x ->
    let rec f n x k =
      if n <= 0 then k [] else f (n - 1) x @@ fun l -> k @@ (x :: l)
    in
    f n x id

  let rev_cons : 'a list -> 'a list * 'a = function
    | [] -> failwith "rev_cons"
    | l ->
        let l' = rev l in
        (rev @@ tl @@ l', hd l')

  let rec last : 'a list -> 'a = function
    | [] -> failwith "last"
    | h :: [] -> h
    | _ :: tl -> last tl

  let cont_l : (('a -> 'w) -> 'w) list -> ('a list -> 'w) -> 'w =
   fun l k ->
    let rec f l k =
      match l with
      | [] -> k []
      | x :: tl ->
          x @@ fun y ->
          f tl @@ fun ys -> k @@ (y :: ys)
    in
    f l k

  let fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a =
   fun f l ->
    match l with [] -> failwith "fold_right1" | x :: xs -> fold_right f xs x

  let assoc_snd : 'b -> ('a * 'b) list -> 'a =
   fun x l -> List.assoc x @@ List.map (fun (x, y) -> (y, x)) l

  let concat_option : 'a option list -> 'a list =
   fun l ->
    List.fold_right
      (fun x acc -> match x with Some v -> v :: acc | None -> acc)
      l []

  let product : 'a list -> 'b list -> ('a * 'b) list =
   fun l1 l2 ->
    List.concat @@ List.map (fun x -> List.map (fun y -> (x, y)) l2) l1
end

module String = struct
  include String

  let insert_at : string -> int -> string -> string =
   fun s n t ->
    let len = String.length s in
    if n < 0 || len < n then invalid_arg "insert_at"
    else String.sub s 0 n ^ t ^ String.sub s n (len - n)

  let pp fmt x = fprintf fmt "%s" x
  let show x = x
end

module IO = struct
  (* file *)
  let count_lines fn =
    let ic = open_in fn in
    let rec f n =
      try
        ignore @@ input_line ic;
        f (n + 1)
      with End_of_file -> n
    in
    let res = f 0 in
    close_in ic;
    res

  let read_lines fn =
    let ic = open_in fn in
    let rec f acc = try f @@ (input_line ic :: acc) with End_of_file -> acc in
    let res = f [] in
    close_in ic;
    List.rev res

  let read_file fn =
    let ic = open_in fn in
    let rec f acc = try f @@ (input_line ic :: acc) with End_of_file -> acc in
    let res = f [] in
    close_in ic;
    String.concat "\n" @@ List.rev res

  let write_file fn s =
    let oc = open_out fn in
    output_string oc s;
    close_out oc

  let append_file fn s =
    let oc = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 fn in
    output_string oc s;
    close_out oc

  let read_file_lines fn bg ed =
    let ic = open_in fn in
    let rec f acc n =
      try
        let l = input_line ic in
        if n < bg then f acc (n + 1)
        else if n <= ed then f (l :: acc) (n + 1)
        else acc
      with End_of_file -> acc
    in
    let res = f [] 0 in
    close_in ic;
    List.rev res

  let read_file_line_at fn l =
    let ic = open_in fn in
    let rec f n =
      try
        let res = input_line ic in
        if n = l then res else f (n + 1)
      with End_of_file -> failwith "read_file_line"
    in
    let res = f 0 in
    close_in ic;
    res

  (* directory *)

  let mkdir fp = if Sys.file_exists fp then () else Unix.mkdir fp 0o777

  let readdir_to_path fp =
    Array.map (String.cat fp **. String.cat "/") @@ Sys.readdir fp

  let rec read_dir_rec fp =
    if Sys.is_directory fp then
      Array.concat @@ Array.to_list @@ Array.map read_dir_rec
      @@ readdir_to_path fp
    else [| fp |]

  let eval_loop k =
    while true do
      k @@ input_line stdin
    done
end

module Tuple2 = struct
  type ('a, 'b) t = 'a * 'b

  let tuple x y = (x, y)
  let map_fst f (x, y) = (f x, y)
  let map_snd f (x, y) = (x, f y)
  let curry f x y = f (x, y)
  let uncurry f (x, y) = f x y
end

module Tuple3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let tuple x y z = (x, y, z)
  let map_fst f (x, y, z) = (f x, y, z)
  let map_snd f (x, y, z) = (x, f y, z)
  let map_thd f (x, y, z) = (x, y, f z)
end

(* Set/Map *)
module Set = struct
  module type I = sig
    include Set.OrderedType
    include Show.I with type t := t
  end

  module type S = sig
    include Set.S

    val of_list : elt list -> t
    val to_list : t -> elt list
    val unions : t list -> t

    include Show.S with type t := t
  end

  module Make (O : I) : S with type elt = O.t = struct
    module SM = Set.Make (O)
    include SM

    module I = struct
      type t = SM.t

      let pp fmt x =
        fprintf fmt "@[<hov 1>{%a}@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") O.pp)
        @@ elements x
    end

    include Show.Make (I)
    include I

    let of_list = List.fold_left (fun s x -> add x s) empty
    let to_list = elements
    let unions = List.fold_left union empty
  end
end

module Format = struct
  include Format

  let pp_list : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit =
   fun pp fmt l ->
    fprintf fmt "@[[%a]@]"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ";@ ") pp)
      l

  let pp_list_comma : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
      =
   fun pp fmt l ->
    fprintf fmt "@[[%a]@]"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ",@ ") pp)
      l

  let pp_array : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit =
   fun pp fmt l ->
    fprintf fmt "@[[|%a|]@]"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ";@ ") pp)
      l

  let pp_tuple : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit =
   fun pp fmt l ->
    fprintf fmt "@[%a@]"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ",@ ") pp)
      l
end

module Result = struct
  include Result

  let get_ok_exn : type e.
      (module Show.S with type t = e) -> ('a, e) result -> 'a =
   fun (module S) r ->
    match r with
    | Ok v -> v
    | Error e ->
        eprintf "%a\n" S.pp e;
        exit 1
end

module SourcePosition = struct
  open String
  open Lexing

  module LexingPosition = struct
    type t = position

    let pp fmt x =
      Format.fprintf fmt "%s:%d:%d" x.pos_fname x.pos_lnum
        (x.pos_cnum - x.pos_bol)

    let show = asprintf "%a" pp
  end

  type t = { start : LexingPosition.t; end_ : LexingPosition.t }

  let dummy : t = { start = Lexing.dummy_pos; end_ = Lexing.dummy_pos }

  let pp fmt (x : t) =
    let _highlighted_position_of_source fname pos_b pos_e =
      if fname = "" then
        fprintf fmt "File \"?\", line %d, characters %d-%d:\n" pos_e.pos_lnum
          (pos_e.pos_cnum - pos_e.pos_bol)
          pos_e.pos_cnum
      else
        let txt = IO.read_file fname in
        fprintf fmt "File \"%s\", line %d, characters %d-%d:\n%s\n" fname
          pos_e.pos_lnum
          (pos_e.pos_cnum - pos_e.pos_bol)
          pos_e.pos_cnum
          (insert_at
             (insert_at txt pos_e.pos_cnum "\027[0m")
             pos_b.pos_cnum "\027[1m\027[31m")
    in
    _highlighted_position_of_source x.start.pos_fname x.start x.end_

  let show = asprintf "%a" pp
end

module Annotated = struct
  type ('body, 'attr) t = { body : 'body; attr : 'attr } [@@deriving show]

  let map f { body; attr } = { body = f body; attr }
  let fold f init { body; _ } = f init body
  let _body { body; _ } = body
  let _attr { attr; _ } = attr
end
