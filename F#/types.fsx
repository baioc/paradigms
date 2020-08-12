#light "off"

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
          else this.Real.ToString() + "+" + this.Imag.ToString() + "i";

        static member zero = { Real = 0.0; Imag = 0.0 };
        static member i = { Complex.zero with Imag = 1.0 };
    end;;

type 't Sexp =
    | Nil
    | Atom of 't
    | Pair of 't Sexp * 't Sexp;;

let rec string sexp =
    match sexp with
    | Nil -> "()"
    | Atom(symbol) -> symbol // implies 't == string
    | Pair(car, cdr) -> "(" + (string car) + " . " + (string cdr) + ")";;


let example = Pair(Atom("+"), Pair(Atom("1"), Pair(Atom("1"), Nil)));;