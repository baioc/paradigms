#light "off"

namespace Basic


type Lexer = { Source: string; Cursor: int; }
    with
        member this.Length = String.length this.Source;
    end;;

type Token =
    | Natural of int
    | String of string
    | Word of string
    | Operator of string
    | LeftParenthesis
    | RightParenthesis
    | EndOfText;;


module Lexer = begin

    /// Initializes a lexer from a given input source.
    let make source =
        { Source = source; Cursor = 0 };;

    /// Looks at what's under the lexer's cursor, if it's valid.
    let internal peek lexer =
        if lexer.Cursor < lexer.Length
        then Some lexer.Source.[lexer.Cursor]
        else None;;

    /// Steps the lexer position forward.
    let internal step lexer =
        { lexer with Cursor = lexer.Cursor + 1 };;

    /// Steps the lexer to its end or until a given predicate returns false,
    /// extracting the string which was skipped over along the way.
    let internal extractWhile pred lexer =
        let rec loop lexer =
            match peek lexer with
            | Some char -> if pred char then loop (step lexer) else lexer.Cursor
            | None -> lexer.Cursor in

        let start, finish = lexer.Cursor, loop lexer in
            { lexer with Cursor = finish }, lexer.Source.[ start .. finish - 1 ];;

    /// Advance to the next lexer state after lexing a token.
    let rec advance lexer =
        let lexWith c =
            match c with
            | ' ' | '\t' -> advance (step lexer)
            | '(' -> step lexer, Ok LeftParenthesis
            | ')' -> step lexer, Ok RightParenthesis
            | c when System.Char.IsDigit c ->
                let lexer, digits = extractWhile System.Char.IsDigit lexer in
                lexer, Ok (Natural (int digits))
            | c when System.Char.IsLetter c ->
                let lexer, id =
                    extractWhile (fun c -> c = '_'
                                        || System.Char.IsLetterOrDigit c)
                                 lexer in
                lexer, Ok (Word id)
            | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '!' | '=' ->
                step lexer, Ok (Operator (string c))
            | '<' | '>' ->
                let lexer = step lexer in
                (match c, peek lexer with
                 | _, None -> lexer, Ok (Operator (string c))
                 | '<', Some '=' -> step lexer, Ok (Operator "<=")
                 | '>', Some '=' -> step lexer, Ok (Operator ">=")
                 | '<', Some '>' -> step lexer, Ok (Operator "<>")
                 | _ -> lexer, Ok (Operator (string c)))
            | '"' ->
                let lexer, str = extractWhile ((<>) '"') (step lexer) in
                (match peek lexer with
                 | Some '"' -> step lexer, Ok (String str)
                 | Some _ | None ->
                    lexer, Error "text ends before closing string quotes")
            | c ->
                step lexer, Error (sprintf "found unexpected character '%c'" c) in
        match peek lexer with
        | None -> lexer, Ok EndOfText
        | Some char -> lexWith char;;

    /// Generates a token sequence from given source string.
    let tokenize source =
        Seq.unfold (fun lexer ->
                        match advance lexer with
                        | _, Error msg -> failwith msg
                        | _, Ok EndOfText -> None
                        | lexer, Ok token -> Some(token, lexer))
                   (make source);;

end;;
