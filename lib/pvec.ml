(* The constant values used.
   2 ^ exp = branching *)
module CS = struct
  let branching = 32
  let exp = 5
  let mask = branching - 1
end

(* A node is either a leaf node containing `branching` values or
   an internal node containing up to `branching` children. We allocate
   children nodes lazily, and since `node` is a boxed type, `node option`
   should represent `None` as a null so no further indirection is happening. *)
type 'a node =
  | Leaf of 'a array
  | Internal of 'a node option array

(* - `root`:
     The root of the tree, which is `None` when just the
     tail has elements to optimise for small vectors.
   - `tail`:
     The tail of the vector; this is an extra array that
     we keep a direct reference to so we can have true
     O(1) appends most of the time. When it's full, it
     gets added to the first available space under the
     root.
   - `depth`, `size`:
     Keeping both the depth and the size is needed so we
     don't needlessly traverse the whole tree for simple
     operations.

   This is the same size as Clojure's implementation. *)
type 'a t =
  { root : 'a node option
  ; tail : 'a array
  ; depth : int
  ; size : int
  }

let alloc_internal () : 'a node option array = Array.init CS.branching (fun _ -> None)
let empty () : 'a t = { root = None; tail = [||]; depth = 0; size = 0 }
let length (vec : 'a t) : int = vec.size

(* `tailoff` is the tail offset of the vector. Intuitively, if we lookup
   index `i` and `i >= tailoff`, the `i`th element is in the tail. *)
let tailoff (vec : 'a t) : int = vec.size - Array.length vec.tail

let index_in_level ~(index : int) ~(level : int) : int =
  (index lsr (level * CS.exp)) land CS.mask
;;

let get (index : int) (vec : 'a t) : 'a =
  let rec get_radix (nd : 'a node) (level : int) : 'a =
    match nd with
    | Leaf xs -> xs.(index land CS.mask)
    | Internal ns ->
      let level_idx = index_in_level ~index ~level in
      get_radix (Option.get ns.(level_idx)) (level - 1)
  in
  if index >= vec.size then failwith "out of bounds";
  if index >= tailoff vec
  then vec.tail.(index land CS.mask)
  else get_radix (Option.get vec.root) vec.depth
;;

let to_string (f : 'a -> string) (vec : 'a t) : string =
  "["
  ^ (List.init vec.size (fun i -> f (get i vec)) |> String.concat ", ")
  ^ "] ("
  ^ Int.to_string vec.depth
  ^ ")"
;;

let update (index : int) (elem : 'a) (vec : 'a t) : 'a t =
  (* For updates, we recursively build a chain down to the
     leaf node we want the update to happen in, sharing the
     other nodes with the original vector *)
  let rec update_node (nd : 'a node) (level : int) : 'a node =
    match nd with
    | Leaf xs ->
      let xs = Array.copy xs in
      Array.set xs (index land CS.mask) elem;
      Leaf xs
    | Internal ns ->
      let ns = Array.copy ns in
      let level_idx = index_in_level ~index ~level in
      let updated = update_node (Option.get ns.(level_idx)) (level - 1) in
      Array.set ns level_idx (Some updated);
      Internal ns
  in
  if index >= vec.size then failwith "out of bounds";
  if index >= tailoff vec
  then (
    (* If the element is in the tail we only have to update the tail *)
    let new_tail = Array.copy vec.tail in
    Array.set new_tail (index land CS.mask) elem;
    { root = vec.root; tail = new_tail; depth = vec.depth; size = vec.size })
  else
    { root = Some (update_node (Option.get vec.root) vec.depth)
    ; tail = vec.tail
    ; depth = vec.depth
    ; size = vec.size
    }
;;

let rec new_path (level : int) (node : 'a node) : 'a node =
  if level = 0
  then node
  else (
    let ret = alloc_internal () in
    Array.set ret 0 (Some (new_path (level - 1) node));
    Internal ret)
;;

let rec push_tail (index : int) (level : int) (node : 'a node) (tail : 'a node) : 'a node =
  (* We never reach a leaf node *)
  let ns =
    match node with
    | Internal ns -> Array.copy ns
    | Leaf _ -> failwith "unreachable"
  in
  let level_idx = index_in_level ~index ~level in
  if level = 1
  then (
    Array.set ns level_idx (Some tail);
    Internal ns)
  else (
    let child = ns.(level_idx) in
    let to_insert =
      match child with
      | None -> new_path (level - 1) tail
      | Some child -> push_tail index (level - 1) child tail
    in
    Array.set ns level_idx (Some to_insert);
    Internal ns)
;;

let push_back (elem : 'a) (vec : 'a t) : 'a t =
  if Array.length vec.tail < CS.branching
  then (
    (* We have space in the tail, so we just make a new one. *)
    let old_tail_len = Array.length vec.tail in
    let new_tail = Array.init (old_tail_len + 1) (fun _ -> Obj.magic ()) in
    Array.blit vec.tail 0 new_tail 0 old_tail_len;
    Array.set new_tail old_tail_len elem;
    { vec with tail = new_tail; size = vec.size + 1 })
  else if vec.size = CS.branching
  then (
    (* The tail is full and we haven't allocated a root yet.
       We allocate a root and put the tail as the left-most leaf. *)
    let new_root = alloc_internal () in
    Array.set new_root 0 (Some (Leaf vec.tail));
    { root = Some (Internal new_root)
    ; tail = [| elem |]
    ; depth = vec.depth + 1
    ; size = vec.size + 1
    })
  else if vec.size - CS.branching = 1 lsl ((vec.depth + 1) * CS.exp)
  then (
    (* The tail is full and the tree is fully saturated.
       We make a new root, put the old root as the left-most
       sub-tree, and make a path of `vec.depth` with the tail
       as its left-most leaf. *)
    let new_root = alloc_internal () in
    Array.set new_root 0 vec.root;
    Array.set new_root 1 (Some (new_path vec.depth (Leaf vec.tail)));
    { root = Some (Internal new_root)
    ; tail = [| elem |]
    ; depth = vec.depth + 1
    ; size = vec.size + 1
    })
  else (
    (* The tail is full and the tree is not fully saturated.
       Since we add `branching` chunks from the tail to the
       tree, we have at least one completely empty leaf. We
       put the tail there. *)
    let new_root =
      push_tail (vec.size - 1) vec.depth (Option.get vec.root) (Leaf vec.tail)
    in
    { root = Some new_root; tail = [| elem |]; depth = vec.depth; size = vec.size + 1 })
;;
