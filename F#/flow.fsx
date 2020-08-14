#light "off"

type Point = { mutable X: float; mutable Y: float; };;
let p = { X = 1.0; Y = 2.0 };;
p.X <- 3.0;;

let v = [| 1.0; 2.0; 3.0; |];;
let m = Array.create 3 v;; // shallow copy
m.[0].[0] <- 0.0;;

let x = ref 3;;
x := !x + 1;;

for i = 10 downto 1 do
    printf "%i " i;
done;
printf "\n";;

let r = ref 1 in
    while !r < 11  do
        printf "%i " !r;
        r := !r + 1;
    done;
printf "\n";;
