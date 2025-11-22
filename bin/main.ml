open Core_bench

let pushes = 1000000

let test_pvec () =
  let push_many (n : int) (v : int Pvec.t) : int Pvec.t =
    let rec aux i v = if i = n then v else aux (i + 1) (Pvec.push_back i v) in
    aux 0 v
  in
  let get_many (n : int) (v : 't Pvec.t) : unit =
    for i = 0 to n - 1 do
      Pvec.get i v |> ignore
    done
  in
  Gc.full_major ();
  Random.self_init ();
  let v = Pvec.empty () in
  let v = push_many pushes v in
  get_many pushes v;
  for _ = 0 to 1000 do
    let r = Random.int pushes in
    Pvec.update r r v |> ignore
  done
;;

let test_hector () =
  let push_many (n : int) (v : int Hector.Poly.t) : unit =
    let rec aux i =
      if i = n
      then ()
      else (
        Hector.Poly.push v i;
        aux (i + 1))
    in
    aux 0
  in
  let get_many (n : int) (v : 't Hector.Poly.t) : unit =
    for i = 0 to n - 1 do
      Hector.Poly.get v i |> ignore
    done
  in
  Gc.full_major ();
  Random.self_init ();
  let v = Hector.Poly.create () in
  push_many pushes v;
  get_many pushes v;
  for _ = 0 to 1000 do
    let r = Random.int pushes in
    Hector.Poly.set v r r
  done
;;

module IntKey : PatriciaTree.KEY with type t = int = struct
  type t = int

  let to_int x = x
end

module IMap : PatriciaTree.MAP with type key = int = PatriciaTree.MakeMap (IntKey)

let test_patricia () =
  let push_many (n : int) (v : int IMap.t) : int IMap.t =
    let rec aux i v = if i = n then v else aux (i + 1) (IMap.add i i v) in
    aux 0 v
  in
  let get_many (n : int) (v : 't IMap.t) : unit =
    for i = 0 to n - 1 do
      IMap.find i v |> ignore
    done
  in
  Gc.full_major ();
  Random.self_init ();
  let v = IMap.empty in
  let v = push_many pushes v in
  get_many pushes v;
  for _ = 0 to 1000 do
    let r = Random.int pushes in
    IMap.add r r v |> ignore
  done
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"pvec" (fun () -> test_pvec ())
       ; Bench.Test.create ~name:"hector" (fun () -> test_hector ())
       ; Bench.Test.create ~name:"patricia" (fun () -> test_patricia ())
       ])
;;
