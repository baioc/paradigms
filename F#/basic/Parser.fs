#light "off"

namespace Basic


(*
    A BASIC operator-precedence grammar.
    Higher precedence values mean stronger affinity to operands.
*)

type UnaryOperator =
    | Negative
    | Not
    with
        static member precedence = function
            | Negative -> 7
            | Not -> 1;
    end;;

type BinaryOperator =
    | Plus | Minus | Multiplication | Division | Modulo
    | Equal | Less | Greater | LessEqual | GreaterEqual | Different
    | And | Or
    with
        static member precedence = function
            | Multiplication | Division -> 6
            | Plus | Minus -> 5
            | Modulo -> 4
            | Equal | Less | Greater | LessEqual | GreaterEqual | Different -> 3
            | And | Or -> 2;
    end;;

type Expression =
    | Number of int
    | Variable of string
    | Text of string
    | Prefix of UnaryOperator * Expression
    | Infix of Expression * BinaryOperator * Expression;;

type Command =
    | Remark of string
    | Goto of int
    | Let of string * Expression
    | Print of Expression
    | Input of string
    | If of Expression * int
    | Gosub of int
    | Return;;

type Line = Line of int * Command;;

type Directive =
    | Code of Line
    | Run
    | List
    | End
    | Save of string
    | Load of string
    | New
    | Help
    | Clear
    | Noop;;


module Parser = begin

    (* pretty-printing ASTs, or the reverse process of parsing them *)

    /// Converts an arbitrary expression to a string (eg. for PRINT).
    let internal showExpression =
        let parenthesize str =
            "(" + str + ")" in

        let showUnary = function
            | Negative -> "-"
            | Not -> "!" in

        let showBinary = function
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
            | Or -> " | " in

        /// pred is used to determine if parenthesis is needed.
        let rec showInfix pred (lhs, op, rhs) =
            let newPrecedence = BinaryOperator.precedence op in
            let result = (showLeft newPrecedence lhs)
                       + (showBinary op)
                       + (showRight newPrecedence rhs) in
            if pred newPrecedence then parenthesize result else result

        // normally, we only need parenthesize when the outer expression's
        // precedence is greater than that of the inner one
        and showLeft outerPrec = function
            | Number i -> string i
            | Variable v -> v
            | Text s -> "\"" + s + "\""
            | Prefix(op, expr) ->
                let innerPrec = UnaryOperator.precedence op in
                let result = (showUnary op) + (showLeft innerPrec expr) in
                if outerPrec >= innerPrec then parenthesize result else result
            | Infix(lhs, op, rhs) -> showInfix ((>) outerPrec) (lhs, op, rhs)

        // right subtrees are different because, due to left-associativity,
        // we also need to parenthesize when precedences are equal
        and showRight outerPrec = function
            | Infix(lhs, op, rhs) -> showInfix ((>=) outerPrec) (lhs, op, rhs)
            | expr -> showLeft outerPrec expr in

        showLeft 0;;

    let internal showCommand = function
        | Remark remark -> "REM " + remark
        | Goto line -> "GOTO " + (string line)
        | Let(var, expr) -> "LET " + var + " = " + (showExpression expr)
        | Print expr -> "PRINT " + (showExpression expr)
        | Input var -> "INPUT " + var
        | If(e, n) -> "IF " + (showExpression e) + " THEN " + (string n)
        | Gosub line -> "GOSUB " + (string line)
        | Return -> "RETURN";;

    let internal showLine = function Line(num, cmnd) ->
        sprintf "%03i  %s" num (showCommand cmnd);;


    (* functional bottom-up LR(1) operator-precedence parser *)

    type private Operator =
        | Bin of BinaryOperator
        | Unr of UnaryOperator
        | Custom of string;;

    type private StackElement =
        | Expr of Expression
        | Op of Operator
        | LParen
        | Err of string;;

    type private Stack = Stack of StackElement list;;

    exception private InvalidReductionException;;

    /// Replaces a valid handle on the stack for its nonterminal rule head,
    /// but doing so only if its precedence is higher than the given threshold.
    let private reduce minPrec (Stack stack) =
        match stack with
        | Expr expr :: Op (Unr op) :: stack
            when UnaryOperator.precedence op >= minPrec ->
                Stack (Expr (Prefix(op, expr)) :: stack)
        | Expr rhs :: Op (Bin op) :: Expr lhs :: stack
            when BinaryOperator.precedence op >= minPrec ->
                Stack (Expr (Infix(lhs, op, rhs)) :: stack)
        | _ -> raise InvalidReductionException;;

    /// Shifts a lookahead symbol into a (possibly recursively reduced) stack.
    let rec private shiftReduce lookahead (Stack stack) =
        let parseOperator = function
            | "!"  -> Unr Not
            | "-"  -> Bin Minus // this is ambiguous, could be an unary negate
            | "+"  -> Bin Plus
            | "*"  -> Bin Multiplication
            | "/"  -> Bin Division
            | "%"  -> Bin Modulo
            | "="  -> Bin Equal
            | "<"  -> Bin Less
            | ">"  -> Bin Greater
            | "<=" -> Bin LessEqual
            | ">=" -> Bin GreaterEqual
            | "<>" -> Bin Different
            | "&"  -> Bin And
            | "|"  -> Bin Or
            | op -> Custom op in

        match lookahead, stack with
        // shift non-terminals
        | Natural num, _ -> Stack (Expr (Number num) :: stack)
        | String txt, _ -> Stack (Expr (Text txt) :: stack)
        | Word var, _ -> Stack (Expr (Variable var) :: stack)
        | LeftParenthesis, _ -> Stack (LParen :: stack)
        // either try reducing by <expr> := "(" <expr> ")" or shift an error
        | RightParenthesis, Expr expr :: LParen :: s -> Stack (Expr expr :: s)
        | RightParenthesis, [] -> Stack ([ Err "mismatched ')'" ])
        | RightParenthesis, _ -> shiftReduce lookahead (reduce 0 (Stack stack))
        // shunting yard
        | Operator sym, _ ->
            let operator =
                if sym = "-" then
                    // resolves ambiguity of '-' operator by looking at
                    // the stack to see if there's a left-hand side operand
                    match stack with
                    | Expr _ :: _ -> Bin Minus
                    | _ -> Unr Negative
                else
                    parseOperator sym in
            (match operator with
             | Unr _ -> Stack (Op operator :: stack) // shift unary operators
             | Bin op ->
                (try // to reduce left operand, otherwise shift operator
                    shiftReduce lookahead
                                (reduce (BinaryOperator.precedence op)
                                        (Stack stack))
               with
               | InvalidReductionException -> Stack (Op operator :: stack))
             | Custom op ->
                Stack (Err (sprintf "undefined operator '%s'" op) :: stack))
        // eot
        | EndOfText, _ -> Stack stack;;

    exception internal ParsingException of string;;

    /// Drives the parser until EOT lex or when given predicate signals to stop.
    let internal parseExpression stopAt lexer =
        let rec reduceAll = function
            | Stack [ Expr root ] -> root
            | Stack s -> reduceAll (reduce 0 (Stack s)) in

        let rec loop lexer stack =
            match stack, Lexer.advance lexer with
            | Stack (Err msg :: _), (_, _) -> raise (ParsingException msg)
            | Stack _, (_, Error msg) -> raise (ParsingException msg)
            | Stack _, (_, Ok EndOfText) -> lexer, reduceAll stack
            | Stack _, (_, Ok tok) when stopAt tok -> lexer, reduceAll stack
            | Stack _, (lexer, Ok tok) -> loop lexer (shiftReduce tok stack) in

        loop lexer (Stack []);;

    let internal parseCommand lexer =
        match Lexer.advance lexer with
        | lexer, Ok (Word "REM") ->
            let _, r = Lexer.extractWhile (fun _ -> true) lexer in Remark r
        | lexer, Ok (Word "PRINT") ->
            let _, expr = parseExpression ((=) EndOfText) lexer in Print expr
        | lexer, Ok (Word "INPUT") ->
            (match Lexer.advance lexer with
             | _, Ok (Word var) -> Input var
             | _, _ -> raise (ParsingException "missing INPUT variable"))
        | lexer, Ok (Word "GOTO") ->
            (match Lexer.advance lexer with
             | _, Ok (Natural target) -> Goto target
             | _, _ -> raise (ParsingException "invalid GOTO jump target"))
        | lexer, Ok (Word "LET") ->
            let lexer, var = Lexer.advance lexer in
            let lexer, eq = Lexer.advance lexer in
            (match var, eq with
             | Ok (Word var), Ok (Operator "=") ->
                let _, expr = parseExpression ((=) EndOfText) lexer in
                Let(var, expr)
             | Ok (Word _), _ ->
                raise (ParsingException "missing '=' after LET variable")
             | _, _ ->
                raise (ParsingException "ill-formed LET"))
        | lexer, Ok (Word "IF") ->
            let lexer, expr = parseExpression ((=) (Word "THEN")) lexer in
            let lexer, _ = Lexer.advance lexer in
                (match Lexer.advance lexer with
                 | _, Ok (Natural branch) -> If(expr, branch)
                 | _, _ -> raise (ParsingException "invalid IF branch target"))
        | lexer, Ok (Word "GOSUB") ->
            (match Lexer.advance lexer with
             | _, Ok (Natural target) -> Gosub target
             | _, _ -> raise (ParsingException "invalid GOSUB subroutine target"))
        | _, Ok (Word "RETURN") -> Return
        | _, token ->
            raise (ParsingException (sprintf "unknown command \"%A\"" token));;

    /// Parses a BASIC directive from an input line.
    let parse userInput =
        match Lexer.advance (Lexer.make userInput) with
        | lexer, Ok (Natural line) ->
            (try Ok (Code (Line(line, parseCommand lexer)))
             with ParsingException msg -> Error (sprintf "%s in line %i" msg line))
        | _, Ok (Word "RUN") -> Ok Run
        | _, Ok (Word "LIST") -> Ok List
        | _, Ok (Word "END") -> Ok End
        | lexer, Ok (Word "SAVE") ->
            (match Lexer.advance lexer with
             | _, Ok (String path) -> Ok (Save path)
             | _, _ -> Error "bad format for SAVE file path")
        | lexer, Ok (Word "LOAD") ->
            (match Lexer.advance lexer with
             | _, Ok (String path) -> Ok (Load path)
             | _, _ -> Error "bad format for LOAD file path")
        | _, Ok (Word "NEW") -> Ok New
        | _, Ok (Word "HELP") -> Ok Help
        | _, Ok (Word "CLEAR") -> Ok Clear
        | _, Ok EndOfText -> Ok Noop
        | _, Error msg -> Error msg
        | _, token -> Error (sprintf "unsupported directive \"%A\"" token);;

end;;
