type 'a t

val empty : 'a t

val generate_randomly_filled_int_grid :
  int -> int -> int -> int -> int t
(** [randomly_filled_int_grid r c l u] is a grid with [r] rows and [c]
    columns with every element in it in between [l] and [u] inclusive. *)

val get : 'a t -> int -> int -> 'a
(** [get g r c] is the equivalent of [g\[r\]\[c\]] in Java. *)

val update_grid : 'a t -> int -> int -> 'a -> 'a t
(** [update_grid g r c e] is a new grid that is the same as [g] but with
    [e] located at row [r] column [c]. *)

val generate_all_false_grid : int -> int -> bool t
(** [generate_all_false_grid r c] is a new grid with [r] rows and [c]
    columns that is filled with all falses. *)
