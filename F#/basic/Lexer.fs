#light "off"

namespace Basic


type Lexer =
    { Source: string;
      Cursor: int;
      Length: int; };;

type Token =
    | Constant of int
    | Word of string
    | Symbol of string
    | String of string
    | End
    | Error of string * int;;


/// A purely functional BASIC lexer.
module Lexer = begin

    let private extractWhile pred lexer =
        match lexer with
        { Source = src; Length = len; Cursor = start } ->
            let rec extractFrom current = if current < len && pred src.[current]
                                          then extractFrom (current + 1)
                                          else current in
            let finish = extractFrom start in
                { lexer with Cursor = finish }, src.[start .. finish - 1];;

    let internal isDigit c = '0' <= c && c <= '9';;
    let internal isLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');;
    let internal isLetterOrDigit c = isLetter c || isDigit c;;

    let private extractInteger lexer =
        let lexer, digits = extractWhile isDigit lexer in lexer, int digits;;

    let private extractWord lexer =
        extractWhile (fun c -> c = '_' || isLetterOrDigit c) lexer;;

    let private peek lexer = lexer.Source.[lexer.Cursor];;

    let private step lexer =
        { lexer with Cursor = min (lexer.Cursor + 1) lexer.Length };;


    /// Builds a lexer from a given string.
    let make (str: string) : Lexer =
        { Source = str; Length = String.length str; Cursor = 0 };;

    /// Get the next lexer state after reading a token.
    let rec next (lexer: Lexer) : Lexer * Token =
        let lexWith c =
            match c with
            | ' ' | '\t' -> next (step lexer)
            | c when isDigit c -> let lexer, int = extractInteger lexer in
                                  lexer, Constant int
            | c when isLetter c -> let lexer, id = extractWord lexer in
                                   lexer, Word id
            | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '!' | '=' | '(' | ')' ->
                (step lexer), Symbol (string c)
            | '<' | '>' ->
                let lexer = step lexer in
                if lexer.Cursor >= lexer.Length then
                    lexer, Symbol (string c)
                else // yup... ML syntax needs this parenthesis
                    (
                        match (c, (peek lexer)) with
                        | ('<', '=') -> (step lexer), Symbol "<="
                        | ('>', '=') -> (step lexer), Symbol ">="
                        | ('<', '>') -> (step lexer), Symbol "<>"
                        | _ -> lexer, Symbol (string c)
                    )
            | '"' -> let lexer, str = extractWhile ((<>) '"') (step lexer) in
                     (step lexer), String str
            | _ -> // instead of throwing, return an Error token and keep going
                (step lexer), Error(lexer.Source, lexer.Cursor) in
        if lexer.Cursor >= lexer.Length
        then lexer, End
        else lexWith (peek lexer);;

    /// Eagerly tokenizes the entire given string.
    let tokenize (str: string) : Token list =
        let rec lex lexer =
            match next lexer with
            | _, End -> []
            | lexer, token -> token :: lex lexer in
        lex (make str);;

end
