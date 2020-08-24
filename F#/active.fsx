(* active patterns *)

let (|Even|Odd|) x = if x % 2 = 0 then Even else Odd

let testNumber x =
    match x with
    | Even -> printfn "%i is even" x
    | Odd -> printfn "%i is odd" x


let (|RGB|) (col : System.Drawing.Color) =
    ( col.R, col.G, col.B )

let (|HSB|) (col : System.Drawing.Color) =
    ( col.GetHue(), col.GetSaturation(), col.GetBrightness() )

let printRGB = function
    RGB(r, g, b) -> printfn " Red: %i Green: %i Blue: %i" r g b

let printHSB = function
    HSB(h, s, b) -> printfn " Hue: %f Saturation: %f Brightness: %f" h s b

let printAll col colorString =
    printfn "%s" colorString
    printRGB col
    printHSB col

printAll System.Drawing.Color.Red "Red"
printAll System.Drawing.Color.Black "Black"
printAll System.Drawing.Color.White "White"
printAll System.Drawing.Color.Gray "Gray"
printAll System.Drawing.Color.BlanchedAlmond "BlanchedAlmond"


(* partial patterns *)

let (|Integer|_|) (str: string) =
   let mutable int = 0
   if System.Int32.TryParse(str, &int) then Some(int)
   else None

let (|Float|_|) (str: string) =
   let mutable floatvalue = 0.0
   if System.Double.TryParse(str, &floatvalue) then Some(floatvalue)
   else None

let parseNumeric str =
   match str with
     | Integer i -> printfn "%d : Integer" i
     | Float f -> printfn "%f : Floating point" f
     | _ -> printfn "%s : Not matched." str

parseNumeric "1.1"
parseNumeric "0"
parseNumeric "0.0"
parseNumeric "10"
parseNumeric "Something else"
