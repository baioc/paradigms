#light "off"

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
