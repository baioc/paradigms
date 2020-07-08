#light "off"

open System

module PigLatin = begin
    let toPigLatin (word: string) =
        let isVowel (c: char) =
            match c with
            | 'a' | 'e' | 'i' |'o' |'u'
            | 'A' | 'E' | 'I' | 'O' | 'U' -> true
            |_ -> false
        in
        if isVowel word.[0] then
            word + "yay"
        else
            word.[1..] + string(word.[0]) + "ay";;
end

[<EntryPoint>]
let main argv = begin
    for name in argv do
        let newName = PigLatin.toPigLatin name in
        printfn "%s in Pig Latin is: %s" name newName;
    done;
    0
end
