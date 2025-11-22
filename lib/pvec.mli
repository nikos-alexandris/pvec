type 'a t

val empty : unit -> 'a t
val length : 'a t -> int
val get : int -> 'a t -> 'a
val update : int -> 'a -> 'a t -> 'a t
val push_back : 'a -> 'a t -> 'a t
val to_string : ('a -> string) -> 'a t -> string
