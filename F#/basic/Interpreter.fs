#light "off"

open Basic


type UnaryOp =
    | Negative
    | Not;;

type BinaryOp =
    | Plus | Minus | Multiplication | Division | Modulo
    | Equal | Less | Greater | LessEqual | GreaterEqual | Different
    | And | Or;;

type Expression =
    | Number of int
    | Variable of string
    | Text of string
    | Unary of UnaryOp * Expression
    | Binary of Expression * BinaryOp * Expression;;

type Command =
    | Rem of string
    | Goto of int
    | Let of string * Expression
    | Print of Expression
    | Input of string
    | If of Expression * int;;

type Line = { Number: int; Command: Command; };;

type Program = Line list;;

type Phrase = Line of Line | Run | List | End;;


(* Operator precedences, higher values mean stronger affinity to operands. *)

let precedenceUnary = function
    | Negative -> 7
    | Not -> 1;;

let precedenceBinary = function
    | Multiplication | Division -> 6
    | Plus | Minus -> 5
    | Modulo -> 4
    | Equal | Less | Greater | LessEqual | GreaterEqual | Different -> 3
    | And | Or -> 2;;


let showUnaryOp = function
    | Negative -> "-"
    | Not -> "!";;

let showBinaryOp = function
    | Plus -> " + "
    | Minus -> " - "
    | Multiplication -> " * "
    | Division -> " / "
    | Modulo -> " % "
    | Equal -> " = "
    | Less -> " < "
    | Greater -> " > "
    | LessEqual -> " <= "
    | GreaterEqual -> " >= "
    | Different -> " <> "
    | And -> " & "
    | Or -> " | ";;

let parenthesize str = "(" + str + ")";;

(*
    Converts arbitrary expressions to a string in order to print source code.
    We do this taking operator precedences into account in order to emit as few
    parentheses as possible, using them only when the outer precedence is
    greater than the inner expression's. When precedence is the same, we also
    need to add parentheses to the right hand side due to left-associativity.
*)
let showExpression =
    let rec showBinaryExpression lhs op rhs cond =
        let newPrecedence = precedenceBinary op in
        let result = (showLeftTree newPrecedence lhs) +
                     (showBinaryOp op) +
                     (showRightTree newPrecedence rhs) in
        if cond newPrecedence then parenthesize result else result
    and showLeftTree precedence = function
        | Number i -> string i
        | Variable v -> v
        | Text s -> "\"" + s + "\""
        | Unary(op, expr) ->
            let newPrecedence = (precedenceUnary op) in
            let result = (showUnaryOp op) + (showLeftTree newPrecedence expr) in
            if precedence >= newPrecedence then parenthesize result else result
        | Binary(lhs, op, rhs) ->
            showBinaryExpression lhs op rhs ((>) precedence)
    and showRightTree precedence = function
        | Binary(lhs, op, rhs) ->
            showBinaryExpression lhs op rhs ((>=) precedence)
        | expr -> showLeftTree precedence expr in
    showLeftTree 0;;

let showCommand = function
    | Rem remark -> "REM " + remark
    | Goto line -> "GOTO " + (string line)
    | Let(var, expr) -> "LET " + var + " = " + (showExpression expr)
    | Print expr -> "PRINT " + (showExpression expr)
    | Input var -> "INPUT " + var
    | If(expr, line) -> "IF " + (showExpression expr) + " THEN " + (string line);;

let showLine l = (string l.Number) + "\t" + (showCommand l.Command);;


[<EntryPoint>]
/// Scans stdin and prints each token correctly lexed therein.
let main argv =

    (*
    [
        Binary(Binary(Number 3,
                      Minus,
                      Number 2),
               Minus,
               Number 1);
        Binary(Number 3,
               Minus,
               Binary(Number 2,
                      Minus,
                      Number 1));
        Unary(Negative,
              Binary(Binary(Number 3,
                            Modulo,
                            Number 4),
                     Multiplication,
                     Number 1));
        Unary(Negative,
              Binary(Number 3,
                     Modulo,
                     Binary(Number 4,
                            Multiplication,
                            Number 1)));
        Binary(Binary(Unary(Negative, Number 3),
                      Modulo,
                      Number 4),
               Multiplication,
               Number 1);
    ]
    |> List.map showExpression
    |> List.iter (printfn "%s");
    *)

    let err = ref false in
    while not !err do
        Lexer.tokenize (System.Console.ReadLine())
        |> List.iter
        (
            function
            | Error(src, pos) ->
                let c = src.[pos] in
                printfn "Lexical failure: found unexpected \'%c\'" c;
                err := true;
            | tok -> printfn "%s" (string tok)
        )
    done;

    0;;
