#light "off"

(* Complements the `lazy` special form. *)
let force (x: Lazy<_>) = x.Force();;

type 'T Stream = // odd streams
    | Cons of 'T * Lazy<'T Stream>
    | Null;;

let head (Cons(h, _)) = h;;
let tail (Cons(_, t)) = force t;;

// an active pattern that unwraps Cons, automatically calling its head and tail
let (|Seq|Nil|) stream =
    match stream with
    | Cons(h, _) -> Seq(h, tail stream)
    | Null -> Nil;;

let rec map f seq =
    match seq with
    | Seq(h,t) -> Cons(f h, lazy (map f t))
    | Nil -> Null;;
