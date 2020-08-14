open System.Drawing


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


printAll Color.Red "Red"
printAll Color.Black "Black"
printAll Color.White "White"
printAll Color.Gray "Gray"
printAll Color.BlanchedAlmond "BlanchedAlmond"
