#light "off"


let isImplication p q =
    match p, q with
    | true, false -> false
    | _ -> true;;

let minRational a b =
    match a, b with
    | ((0,_), b) -> b
    | (a, (0,_)) -> a
    | (((n1,d1) as r1), ((n2,d2) as r2)) -> if n1 * d2 < n2 * d1 then r1 else r2;;

let charDiscriminate c =
    match c with
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'y'
    | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y'  -> "Voyelle"
    | c when 'a' <= c && c < 'z' || 'A' <= c && c < 'Z' -> "Consonne"
    | c when '0' <= c && c <= '9' -> "Chiffre"
    | _ -> "Autre";;

(* `function` is short for a lambda that pattern matches on a single argument *)
let rec foldLeft f acc = function
    | [] -> acc
    | elem :: list -> foldLeft f (f acc elem) list;;


type ('a, 'b) Duple = 'a * 'b;;

type Complex =
    { Real: float;
      Imag: float; }
    with
        static member (+) (z1: Complex, z2: Complex) =
            { Real = z1.Real + z2.Real; Imag = z1.Imag + z2.Imag };

        static member (*) (z1: Complex, z2: Complex) =
            match z1, z2 with { Real = x1; Imag = y1 }, { Real = x2; Imag = y2 } ->
                { Real = x1 * x2 - y1 * y2; Imag = x1 * y2 + x2 * y1 };

        override this.ToString() =
          if this.Imag = 0.0
          then this.Real.ToString()
          else this.Real.ToString() + "+" + this.Imag.ToString() + "i"

        static member zero = { Real = 0.0; Imag = 0.0 };
        static member i = { Complex.zero with Imag = 1.0 };
    end

type 't Sexp =
    | Nil
    | Atom of 't
    | Pair of 't Sexp * 't Sexp;;

let example = Pair(Atom("+"), Pair(Atom("1"), Pair(Atom("1"), Nil)));;

let rec string sexp =
    match sexp with
    | Nil -> "()"
    | Atom(symbol) -> symbol // implies 't == string
    | Pair(car, cdr) -> "(" + (string car) + " . " + (string cdr) + ")";;


exception Custom of string;;

let testExceptions n =
    let f ns = match ns with // failure exception on list with length != 1
        | [1] -> failwith "generic exception"
        | [2] -> raise (Custom "custom exception")
        | [_] -> failwithf "generic fail %d with printf formating" ns.[0] in
    try
        f [n]; ""
    with
        | Failure msg -> "caught: " + msg
        | Custom msg -> "custom: " + msg;;
