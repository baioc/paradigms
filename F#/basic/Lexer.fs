#light "off"

namespace Basic


type Lexer =
    { Source: string;
      Cursor: int;
      Length: int; };;

type Token =
    | Integer of int
    | String of string
    | Word of string
    | Operator of string
    | LeftParenthesis
    | RightParenthesis
    | EndOfText
    | LexError of string;;


/// A purely functional BASIC lexer.
module Lexer =
    begin

        /// Initializes a lexer from a given input.
        let make str =
            { Source = str; Length = String.length str; Cursor = 0 };;

        /// Checks if the lexer has finished analyzing its input.
        let internal isDone lexer = lexer.Cursor >= lexer.Length;;

        /// Looks at what's under the lexer's cursor.
        let internal peek lexer =
            lexer.Source.[lexer.Cursor];;

        /// Steps the lexer position forward.
        let internal step lexer = { lexer with Cursor = lexer.Cursor + 1 };;

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

        /// Lex an integer starting from the given lexer state.
        let internal readInteger lexer =
            let lexer, digits = extractWhile System.Char.IsDigit lexer in
            lexer, Integer (int digits);;

        /// Lex a word (identifier or keyword) from the current lexer state.
        let internal readWord lexer =
            let lexer, id = extractWhile (fun c -> c = '_' ||
                                                   System.Char.IsLetterOrDigit c)
                                         lexer in
            lexer, Word id;;

        /// Advance to the next lexer state after lexing a token.
        let rec advance lexer : Lexer * Token =
            let lexWith c =
                match c with
                | ' ' | '\t' -> advance (step lexer)
                | '(' -> (step lexer), LeftParenthesis
                | ')' -> (step lexer), RightParenthesis
                | c when System.Char.IsDigit c -> readInteger lexer
                | c when System.Char.IsLetter c -> readWord lexer
                | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '!' | '=' ->
                    (step lexer), Operator (string c)
                | '<' | '>' ->
                    let lexer = step lexer in
                    if isDone lexer then
                        lexer, Operator (string c)
                    else
                        ( // yup... ML syntax needs these parentheses
                            match (c, (peek lexer)) with
                            | ('<', '=') -> (step lexer), Operator "<="
                            | ('>', '=') -> (step lexer), Operator ">="
                            | ('<', '>') -> (step lexer), Operator "<>"
                            | _ -> lexer, Operator (string c)
                        )
                | '"' ->
                    let lexer, str = extractWhile (( <> ) '"') (step lexer) in
                    if not (isDone lexer) && (peek lexer) = '"'
                    then (step lexer), String str
                    else ( lexer,
                           LexError "text ends before closing string quotes" )
                | c -> ( (step lexer),
                         LexError (sprintf "found unexpected character '%c'" c) ) in
            if isDone lexer
            then lexer, EndOfText
            else lexWith (peek lexer);;

    end
