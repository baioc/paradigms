#light "off"

open Basic

[<EntryPoint>]
let main argv =
    ignore <| Interpreter.run Interpreter.idleProgram; 0;;
