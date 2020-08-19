#light "off"

namespace Basic


type Lexer = { Source: string; Length: int; Cursor: int; };;

type Token =
    | Positive of int
    | String of string
    | Word of string
    | Operator of string
    | LeftParenthesis
    | RightParenthesis
    | EndOfText;;


module Lexer = begin

    /// Initializes a lexer from a given input source.
    let make source =
        { Source = source; Length = String.length source; Cursor = 0 };;

    /// Checks if the lexer has finished analyzing its input.
    let internal isDone lexer =
        lexer.Cursor >= lexer.Length;;

    /// Looks at what's under the lexer's cursor.
    let internal peek lexer =
        lexer.Source.[lexer.Cursor];;

    /// Steps the lexer position forward.
    let internal step lexer =
        { lexer with Cursor = lexer.Cursor + 1 };;

    /// Steps the lexer to its end or until a given predicate returns false,
    /// extracting the string which was skipped over along the way.
    let internal extractWhile pred lexer =
        match lexer with
        { Source = src; Length = len; Cursor = start } ->
            let rec extractFrom current =
                if current < len && pred src.[current]
                then extractFrom (current + 1)
                else current in
            let finish = extractFrom start in
                ( { lexer with Cursor = finish },
                  src.[ min start (len - 1) .. finish - 1] );;

    /// Advance to the next lexer state after lexing a token.
    let rec advance lexer =
        let lexWith c =
            match c with
            | ' ' | '\t' -> advance (step lexer)
            | '(' -> step lexer, Ok LeftParenthesis
            | ')' -> step lexer, Ok RightParenthesis
            | c when System.Char.IsDigit c ->
                let lexer, digits = extractWhile System.Char.IsDigit lexer in
                lexer, Ok (Positive (int digits))
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
                if isDone lexer then
                    lexer, Ok (Operator (string c))
                else // yup... ML syntax needs these parentheses
                    ( match c, peek lexer with
                      | ('<', '=') -> step lexer, Ok (Operator "<=")
                      | ('>', '=') -> step lexer, Ok (Operator ">=")
                      | ('<', '>') -> step lexer, Ok (Operator "<>")
                      | _ -> lexer, Ok (Operator (string c)) )
            | '"' ->
                let lexer, str = extractWhile (( <> ) '"') (step lexer) in
                if not (isDone lexer) && peek lexer = '"'
                then step lexer, Ok (String str)
                else lexer, Error "text ends before closing string quotes"
            | c ->
                step lexer, Error (sprintf "found unexpected character '%c'" c) in
        if isDone lexer
        then lexer, Ok EndOfText
        else lexWith (peek lexer);;

end;;
