#light "off"

open Basic

[<EntryPoint>]
let main argv =
    match Array.length argv with
    | 0 -> ignore <| Interpreter.run Interpreter.idleProgram; 0
    | 1 -> Interpreter.load argv.[0] |> Interpreter.exec; 0
    | n -> printfn "\
Usage: basic [program]

  A mini BASIC simulator. UPPER CASE only.

  With no arguments, the BASIC system is started on interactive mode.
  If a <program> file is given, it is executed in batch mode."; n;;
