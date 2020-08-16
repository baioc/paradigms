#light "off"

(* `function` is short for a lambda that pattern matches on its arguments *)

let isImplication = function
    | true, false -> false
    | _ -> true;;

let minRational a b = function
    | ((0,_), b) -> b
    | (a, (0,_)) -> a
    | (((n1,d1) as r1), ((n2,d2) as r2)) -> if n1 * d2 < n2 * d1 then r1 else r2;;

let charDiscriminate = function
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'y'
    | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y'  -> "Voyelle"
    | c when 'a' <= c && c < 'z' || 'A' <= c && c < 'Z' -> "Consonne"
    | c when '0' <= c && c <= '9' -> "Chiffre"
    | _ -> "Autre";;

let rec foldLeft f acc = function
    | [] -> acc
    | elem :: list -> foldLeft f (f acc elem) list;;
