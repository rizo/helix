(* Subscription registry for signals. *)
module Subs : sig
  type 'a sub = 'a -> unit
  type 'a t

  val empty : unit -> 'a t
  val add : 'a sub -> 'a t -> unit
  val dispatch : 'a -> 'a t -> unit
end = struct
  type 'a sub = 'a -> unit
  type 'a t = 'a sub list ref

  let empty () = ref []
  let add (k : 'a sub) subs = subs := List.append !subs [ k ]
  let dispatch x subs = List.iter (fun k -> k x) !subs
end

type 'a t = {
  name : string;
  mutable value : 'a;
  emit : 'a -> unit;
  sub : ('a -> unit) -> unit;
}

let base ~name value =
  let subs = Subs.empty () in
  let sub k = Subs.add k subs in
  let rec s = { name; value; emit; sub }
  and emit x =
    s.value <- x;
    Subs.dispatch s.value subs
  in
  s

let make value = base ~name:"make" value

let never =
  let sub _ = () in
  let rec s = { name = "never"; value = (); emit; sub } and emit _x = () in
  s

let get s = s.value
let emit x s = s.emit x
let update f s = s.emit (f s.value)
let sub k s = s.sub k

let sub2 k s1 s2 =
  s1.sub (fun x1 -> k x1 s2.value);
  s2.sub (fun x2 -> k s1.value x2)

let use k s =
  s.sub k;
  k s.value

let use2 k s1 s2 =
  s1.sub (fun x1 -> k x1 s2.value);
  s2.sub (fun x2 -> k s1.value x2);
  k s1.value s2.value

let map f s =
  let s' = base ~name:"map" (f s.value) in
  s.sub (fun x -> s'.emit (f x));
  s'

let const x s =
  let s' = base ~name:"const" x in
  s.sub (fun _ -> s'.emit x);
  s'

let tap f s =
  let s' = base ~name:"tap" s.value in
  s.sub (fun x ->
      f x;
      s'.emit x);
  s'

let pair s1 s2 =
  let subs = Subs.empty () in
  let rec s' = { name = "pair"; value = (s1.value, s2.value); emit; sub }
  and emit x =
    s'.value <- x;
    Subs.dispatch x subs
  and sub k = Subs.add k subs in
  s1.sub (fun x1 -> emit (x1, s2.value));
  s2.sub (fun x2 -> emit (s1.value, x2));
  s'

let triple s1 s2 s3 =
  let subs = Subs.empty () in
  let rec s' =
    { name = "triple"; value = (s1.value, s2.value, s3.value); emit; sub }
  and emit x =
    s'.value <- x;
    Subs.dispatch x subs
  and sub k = Subs.add k subs in
  s1.sub (fun x1 -> emit (x1, s2.value, s3.value));
  s2.sub (fun x2 -> emit (s1.value, x2, s3.value));
  s3.sub (fun x3 -> emit (s1.value, s2.value, x3));
  s'

let filter pred ~seed s =
  let subs = Subs.empty () in
  let sub k = Subs.add k subs in
  let rec s' =
    {
      name = "filter";
      value = (if pred s.value then s.value else seed);
      emit;
      sub;
    }
  and emit x =
    if pred x then (
      s'.value <- x;
      Subs.dispatch s'.value subs)
  in
  s.sub emit;
  s'

let filter_map f ~seed s =
  let subs = Subs.empty () in
  let sub k = Subs.add k subs in
  let rec s' =
    {
      name = "filter_map";
      value =
        (match f s.value with
        | Some x -> x
        | None -> seed);
      emit;
      sub;
    }
  and emit x =
    s'.value <- x;
    Subs.dispatch s'.value subs
  in
  s.sub (fun x ->
      match f x with
      | Some x' -> emit x'
      | None -> ());
  s'

let reduce f init s =
  let subs = Subs.empty () in
  let sub k = Subs.add k subs in
  let rec s' = { name = "reduce"; value = f init s.value; emit; sub }
  and emit x =
    s'.value <- x;
    Subs.dispatch x subs
  in
  s.sub (fun value -> s'.emit (f s'.value value));
  s'

let reducer f init =
  let events = base ~name:"reducer" None in
  let state_signal =
    reduce
      (fun state event_opt ->
        match event_opt with
        | Some event -> f state event
        | None -> state)
      init events
  in
  let dispatch event = emit (Some event) events in
  (state_signal, dispatch)

let select l =
  match l with
  | [] -> invalid_arg "Signal.select: empty signal list"
  | s1 :: _ ->
    let subs = Subs.empty () in
    let sub k = Subs.add k subs in
    let rec s' = { name = "select"; value = s1.value; emit; sub }
    and emit x =
      s'.value <- x;
      Subs.dispatch x subs
    in
    List.iter (fun s -> s.sub emit) l;
    s'

let uniq ?(equal = ( == )) s =
  let subs = Subs.empty () in
  let rec s' = { name = "uniq"; value = s.value; emit; sub }
  and emit x =
    if not (equal x s'.value) then (
      s'.value <- x;
      Subs.dispatch s'.value subs)
  and sub k = Subs.add k subs in
  s.sub emit;
  s'

let map2 f s1 s2 =
  let subs = Subs.empty () in
  let rec s' = { name = "map2"; value = f s1.value s2.value; emit; sub }
  and emit x =
    s'.value <- x;
    Subs.dispatch x subs
  and sub k = Subs.add k subs in
  s1.sub (fun x1 -> emit (f x1 s2.value));
  s2.sub (fun x2 -> emit (f s1.value x2));
  s'

let map3 f s1 s2 s3 =
  let subs = Subs.empty () in
  let rec s' =
    { name = "map3"; value = f s1.value s2.value s3.value; emit; sub }
  and emit x =
    s'.value <- x;
    Subs.dispatch x subs
  and sub k = Subs.add k subs in
  s1.sub (fun x1 -> emit (f x1 s2.value s3.value));
  s2.sub (fun x2 -> emit (f s1.value x2 s3.value));
  s3.sub (fun x3 -> emit (f s1.value s2.value x3));
  s'

let sample ~on:s1 s2 =
  let s' = base ~name:"sample" s2.value in
  s1.sub (fun _ -> s'.emit s2.value);
  s'

let apply f_s x_s =
  let subs = Subs.empty () in
  let rec s' = { name = "apply"; value = f_s.value x_s.value; emit; sub }
  and emit x =
    s'.value <- x;
    Subs.dispatch x subs
  and sub k = Subs.add k subs in
  f_s.sub (fun f -> emit (f x_s.value));
  x_s.sub (fun x -> emit (f_s.value x));
  s'

(* Time *)

let never_equal _ _ = false

let emitter ?(equal = never_equal) ~init emitter =
  let subs = Subs.empty () in
  let sub k = Subs.add k subs in
  let rec s = { name = "emitter"; value = init; emit; sub }
  and emit x =
    if not (equal x s.value) then (
      s.value <- x;
      Subs.dispatch s.value subs)
  in
  emitter emit;
  s

module Syntax = struct
  let ( let+ ) s f = map f s
  let ( and+ ) = pair
  let ( <~ ) = map
  let ( ~~ ) = apply
end