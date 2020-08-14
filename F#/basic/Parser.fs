#light "off"

namespace Basic


(* A BASIC operator-precedence grammar. *)

type UnaryOperator =
    | Negative
    | Not;;

type BinaryOperator =
    | Plus | Minus | Multiplication | Division | Modulo
    | Equal | Less | Greater | LessEqual | GreaterEqual | Different
    | And | Or;;

type Expression =
    | Number of int
    | Variable of string
    | Text of string
    | Prefix of UnaryOperator * Expression
    | Infix of Expression * BinaryOperator * Expression
    | ParseError of string;;

type Command =
    | Remark of string
    | Goto of int
    | Let of string * Expression
    | Print of Expression
    | Input of string
    | If of Expression * int
    | Invalid of string;;

type Line = { Number: int; Command: Command };;

type Directive =
    | Instruction of Line
    | Run
    | List
    | End
    | Help
    | Clear
    | Noop
    | Unknown of string;;


module Parser =
    begin

        (* Higher precedence values mean stronger affinity to operands. *)

        let internal precedenceUnary = function
            | Negative -> 7
            | Not -> 1;;

        let internal precedenceBinary = function
            | Multiplication | Division -> 6
            | Plus | Minus -> 5
            | Modulo -> 4
            | Equal | Less | Greater | LessEqual | GreaterEqual | Different -> 3
            | And | Or -> 2;;


        let private showUnary = function
            | Negative -> "-"
            | Not -> "!";;

        let private showBinary = function
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

        let internal parenthesize str = "(" + str + ")";;

        /// Converts an arbitrary expression to a string (eg. for PRINT), while
        /// considering operator precedences to emit as few parentheses as possible.
        let internal showExpression =
            // cond is a predicate used to determine if parenthesis is needed
            let rec showInfix cond (lhs, op, rhs) =
                let newPrecedence = precedenceBinary op in
                let result = (showLeft newPrecedence lhs) +
                             (showBinary op) +
                             (showRight newPrecedence rhs) in
                if cond newPrecedence then parenthesize result else result

            // normally, we parenthesize when the outer expression's precedence
            // is greater than the inner one
            and showLeft outerPrec = function
                | Number i -> string i
                | Variable v -> v
                | Text s -> "\"" + s + "\""
                | Prefix(op, expr) ->
                    let innerPrec = precedenceUnary op in
                    let result = (showUnary op) + (showLeft innerPrec expr) in
                    if outerPrec >= innerPrec then parenthesize result else result
                | Infix(lhs, op, rhs) -> showInfix ((>) outerPrec) (lhs, op, rhs)
                | ParseError err -> failwith err

            // right subtree is different because, due to left-associativity,
            // we also need to parenthesize when precedences are the same
            and showRight outerPrec = function
                | Infix(lhs, op, rhs) -> showInfix ((>=) outerPrec) (lhs, op, rhs)
                | expr -> showLeft outerPrec expr in

            showLeft 0;;

        let private showCommand = function
            | Remark remark -> "REM " + remark
            | Goto line -> "GOTO " + (string line)
            | Let(var, expr) -> "LET " + var + " = " + (showExpression expr)
            | Print expr -> "PRINT " + (showExpression expr)
            | Input var -> "INPUT " + var
            | If(e, n) -> "IF " + (showExpression e) + " THEN " + (string n)
            | Invalid err -> err;;

        let showLine = function line ->
            (string line.Number) + "\t" + (showCommand line.Command);;


        type internal StackElement =
            | Expr of Expression
            | Op of Operator
            | LParen
            | Err of string
        and Operator =
            | Bin of BinaryOperator
            | Unr of UnaryOperator
            | Custom of string;;

        exception private ReductionException;;

        let private parseOperator = function
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
            | op -> Custom op;;

        /// Replaces a valid handle on the stack for its nonterminal rule head,
        /// but doing so only if its precedence is higher than the given threshold.
        let private reduce minPrec = function
            | Expr expr :: Op (Unr op) :: stack
                when precedenceUnary op >= minPrec ->
                    Expr (Prefix(op, expr)) :: stack
            | Expr rhs :: Op (Bin op) :: Expr lhs :: stack
                when precedenceBinary op >= minPrec ->
                    Expr (Infix(lhs, op, rhs)) :: stack
            | _ -> raise ReductionException;;

        /// LR(1) operator-precedence parsing algorithm.
        let rec internal shiftReduce lookahead stack =
            match lookahead, stack with
            // shift non-terminals
            | Integer num, _ -> Expr (Number num) :: stack
            | String txt, _ -> Expr (Text txt) :: stack
            | Word var, _ -> Expr (Variable var) :: stack
            | LeftParenthesis, _ -> LParen :: stack
            // either try reducing by <expr> ::= "(" <expr> ")" or shift an error
            | RightParenthesis, Expr expr :: LParen :: stack -> Expr expr :: stack
            | RightParenthesis, [] -> [ Err "mismatched ')'" ]
            | RightParenthesis, _ -> shiftReduce lookahead (reduce 0 stack)
            // shunting yard
            | Operator sym, _ ->
                let operator =
                    if sym = "-" then
                        // @XXX: resolves ambiguity of "-" operator by looking at
                        // the stack to see if there's a left-hand side operand
                        match stack with
                        | Expr _ :: _ -> Bin Minus
                        | _ -> Unr Negative
                    else
                        parseOperator sym in
                (
                    match operator with
                    | Unr op -> Op operator :: stack // shift unary operators
                    | Bin op ->
                        (
                            // either reduce left operand or shift operator
                            try
                                shiftReduce lookahead
                                            (reduce (precedenceBinary op) stack)
                            with
                            | ReductionException -> Op operator :: stack
                        )
                    | Custom op ->
                        Err (sprintf "undefined operator '%s'" op) :: stack
                )
            | _, _ -> failwithf "shift-reduce error with %A" (lookahead, stack);;

        /// If the parsed expression is well formed and the terms in one of its
        /// production rule's right side have been shifted to the stack, we can
        /// apply successive reductions until we get to the AST's root.
        let rec private reduceAll = function
            | [ Expr root ] -> root
            | stack -> reduceAll (reduce 0 stack);;

        /// Bottom-up parser driver, at each step extracting a token with the
        /// lexer and passing it in as lookahead to the shift-reduce procedure;
        /// it returns the final AST on EOT or when a given predicate signals to stop.
        let internal parseExpression stop lexer =
            let rec parse lexer stack =
                match stack, Lexer.advance lexer with
                | Err msg :: _, (_, _) -> lexer, ParseError msg
                | _, (_, LexError msg) -> lexer, ParseError msg
                | _, (_, EndOfText) -> lexer, reduceAll stack
                | _, (_, tok) when stop tok -> lexer, reduceAll stack
                | _, (lexer, tok) -> parse lexer (shiftReduce tok stack) in
            parse lexer [];;

        let private parseCommand line lexer =
            let error msg = Invalid (sprintf "%s in line %i" msg line) in
            match Lexer.advance lexer with
            | lexer, Word "REM" ->
                let n = lexer.Length - 1 in
                Remark lexer.Source.[min lexer.Cursor n .. n]
            | lexer, Word "PRINT" ->
                (
                    match parseExpression (( = ) EndOfText) lexer with
                    | _, ParseError err -> error err
                    | _, expr -> Print expr
                )
            | lexer, Word "INPUT" ->
                (
                    match Lexer.advance lexer with
                    | _, Word var -> Input var
                    | _, _ -> error "missing INPUT variable"
                )
            | lexer, Word "GOTO" ->
                (
                    match Lexer.advance lexer with
                    | _, Integer target -> Goto target
                    | _, _ -> error "invalid GOTO jump target"
                )
            | lexer, Word "LET" ->
                let lexer, var = Lexer.advance lexer in
                let lexer, eq = Lexer.advance lexer in
                (
                    match var, eq with
                    | Word var, Operator "=" ->
                        (
                            match parseExpression (( = ) EndOfText) lexer with
                            | _, ParseError msg -> error msg
                            | _, expr -> Let(var, expr)
                        )
                    | Word var, _ -> error "missing '=' after LET variable"
                    | _, _ -> error "wrong LET syntax"
                )
            | lexer, Word "IF" ->
                (
                    match parseExpression (( = ) (Word "THEN")) lexer with
                    | _, ParseError msg -> error msg
                    | lexer, expr ->
                        let lexer, _ = Lexer.advance lexer in
                        (
                            match Lexer.advance lexer with
                            | _, Integer branch -> If(expr, branch)
                            | _, _ -> error "invalid IF branch target"
                        )
                )
            | _, token -> error (sprintf "unknown command \"%A\"" token);;

        let parseDirective input =
            let lexer = Lexer.make input in
            match Lexer.advance lexer with
            | lexer, Integer line ->
                (
                    match parseCommand line lexer with
                    | Invalid err -> Unknown err
                    | command -> Instruction { Number = line; Command = command }
                )
            | _, Word "RUN" -> Run
            | _, Word "LIST" -> List
            | _, Word "END" -> End
            | _, Word "HELP" -> Help
            | _, Word "CLEAR" -> Clear
            | _, EndOfText -> Noop
            | _, LexError err -> Unknown err
            | _, token -> Unknown (sprintf "unknown directive \"%A\"" token);;

    end;;
