#light "off"

open Basic

let toUpper = String.map System.Char.ToUpper;;

[<EntryPoint>]
let rec main argv =
    let input = printf "> "; System.Console.ReadLine() in
    if isNull input then
        0
    else
        match Parser.parseDirective (toUpper input) with
        | Instruction line -> printfn "%s" (Parser.showLine line); main argv
        | Run -> printfn "RUN"; main argv
        | List -> printfn "LIST"; main argv
        | End -> 0
        | Help -> printfn "HELP"; main argv
        | Clear -> printfn "CLEAR"; main argv
        | Noop -> main argv
        | ParseError err -> printfn "SYNTAX ERROR: %s" (toUpper err); main argv;;
