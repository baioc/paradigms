#light "off"


(* Complements the `lazy` special form. *)
let force (x: Lazy<_>) = x.Force();;


type 'T Stream = // odd streams
    | Cons of 'T * Lazy<'T Stream>
    | Null;;

let head = function
    | Cons(h, _) -> h
    | Null -> failwith "empty stream";;

let tail = function
    | Cons(_, t) -> force t
    | Null -> failwith "empty stream";;


let rec unfold gen seed = Cons(seed, lazy (unfold gen (gen seed)));;

let rec map f = function
    | Cons(h,t) -> Cons(f h, lazy (map f (force t)))
    | Null -> Null;;
