#light "off"

namespace Basic


type Value =
    | Int of int
    | Bool of bool
    | String of string
    with
        override this.ToString() =
            match this with
            | Int n -> string n
            | Bool b -> string b
            | String s -> s;
    end;;

type Environment = Environment of Map<string,Value>;;

type Program = Program of Map<int,Command>;;


/// A functional-first BASIC interpreter.
module Interpreter = begin

    /// Initial evaluation environment.
    let emptyEnvironment = Environment (Map [||]);;

    /// Updates environment with a new variable definition.
    let define var value (Environment env) =
        Environment (Map.add var value env);;

    /// Looks up a variable's associated value in an environment.
    let lookUp var (Environment env) =
        Map.tryFind var env;;

    /// Evaluates an expression on a given environment.
    let rec eval expr env =
        match expr with
        | Number num -> Ok (Int num)
        | Text str -> Ok (String str)
        | Variable var ->
          ( match lookUp var env with
            | Some value -> Ok (value)
            | None -> Error (sprintf "use of undefined variable \"%s\"" var) )
        | Prefix(op, expr) ->
          ( match op, eval expr env with
            | Negative, Ok (Int num) -> Ok (Int -num)
            | Not, Ok (Bool bool) -> Ok (Bool (not bool))
            | _, Error msg -> Error msg
            | _, Ok x ->
                Error (sprintf "can't apply operator '%O' to type %A" op x) )
        | Infix(lhs, op, rhs) ->
           match eval lhs env, op, eval rhs env with
            // int x int
            | Ok (Int a), Plus, Ok (Int b) -> Ok (Int (a + b))
            | Ok (Int a), Minus, Ok (Int b) -> Ok (Int (a - b))
            | Ok (Int a), Multiplication, Ok (Int b) -> Ok (Int (a * b))
            | Ok (Int _), Division, Ok (Int 0) ->
                Error "attempted division by zero"
            | Ok (Int a), Division, Ok (Int b) -> Ok (Int (a / b))
            | Ok (Int _), Modulo, Ok (Int 0) ->
                Error "attempted to take modulo 0"
            | Ok (Int a), Modulo, Ok (Int b) -> Ok (Int (a % b))
            | Ok (Int a), Equal, Ok (Int b) -> Ok (Bool (a = b))
            | Ok (Int a), Less, Ok (Int b) -> Ok (Bool (a < b))
            | Ok (Int a), Greater, Ok (Int b) -> Ok (Bool (a > b))
            | Ok (Int a), LessEqual, Ok (Int b) -> Ok (Bool (a <= b))
            | Ok (Int a), GreaterEqual, Ok (Int b) -> Ok (Bool (a >= b))
            | Ok (Int a), Different, Ok (Int b) -> Ok (Bool (a <> b))
            // bool x bool
            | Ok (Bool a), Equal, Ok (Bool b) -> Ok (Bool (a = b))
            | Ok (Bool a), Different, Ok (Bool b) -> Ok (Bool (a <> b))
            | Ok (Bool a), And, Ok (Bool b) -> Ok (Bool (a && b))
            | Ok (Bool a), Or, Ok (Bool b) -> Ok (Bool (a || b))
            // string x string
            | Ok (String a), Plus, Ok (String b) -> Ok (String (a + b))
            | Ok (String a), Equal, Ok (String b) -> Ok (Bool (a = b))
            | Ok (String a), Different, Ok (String b) -> Ok (Bool (a <> b))
            // else
            | Error msg, _, _ -> Error msg
            | _, _, Error msg -> Error msg
            | Ok a, _, Ok b ->
                Error (sprintf "can't apply operator '%O' to types %A and %A" op a b);;


    /// A trivial program which no commands.
    let idleProgram = Program (Map [||]);;

    /// Writes a program line, overriding any previous one with the same label.
    let insert (Line(num, cmnd)) (Program prog) =
        Program (Map.add num cmnd prog);;

    type internal Process =
        { InstructionPointer: int;
          ProgramMemory: seq<Command>;
          DataMemory: Environment; };;

    /// Checks if an instruction address is valid inside a process.
    let internal isValidTarget addr proc =
        0 <= addr && addr < Seq.length proc.ProgramMemory;;

    exception internal RuntimeException of string * Process;;

    /// Executes a single command and steps the process one instruction forward.
    let internal step instr proc =
        let fetchNext proc =
            let next = proc.InstructionPointer + 1 in
            if isValidTarget next proc
            then { proc with InstructionPointer = next }
            else { proc with InstructionPointer = -1 } in

        let set var value proc =
            { proc with DataMemory = define var value proc.DataMemory } in

        let jump addr proc = { proc with InstructionPointer = addr } in

        let eval expr = eval expr proc.DataMemory in

        let runtimeError = raise << RuntimeException in

        match instr with
        | Remark _ -> fetchNext proc
        | Print expr ->
          ( match eval expr with
            | Ok value -> printfn "%O" value; fetchNext proc
            | Error msg -> runtimeError(msg, proc) )
        | Let(var, expr) ->
          ( match eval expr with
            | Ok value -> fetchNext (set var value proc)
            | Error msg -> runtimeError(msg, proc) )
        | Input var ->
          ( try
                let input = System.Console.ReadLine() in
                let value = Int (int input) in
                fetchNext (set var value proc)
            with
            | :? System.FormatException ->
                runtimeError("received non-numeric INPUT", proc) )
        | Goto addr ->
            if isValidTarget addr proc
            then jump addr proc
            else runtimeError("non-existent GOTO target address", proc)
        | If(expr, addr) ->
            match eval expr with
            | Ok (Bool false) -> fetchNext proc
            | Ok (Bool true) ->
                if isValidTarget addr proc
                then jump addr proc
                else runtimeError("non-existent IF target address", proc)
            | Ok v ->
                runtimeError(sprintf "wrong type %A at IF condition" v, proc)
            | Error msg -> runtimeError(msg, proc);;

    /// Assembles a program with unordered and probably sparse line numbers into
    /// a sequence of commands, fixing jump and branch targets on the way.
    let private assemble (Program prog) =
        let assembly =
            Map.toSeq prog |> Seq.indexed in

        let findAddress line =
            Seq.pick (fun (addr, (label, _)) ->
                        if label = line then Some addr else None)
                     assembly in

        let patch (_, (_, cmnd)) =
            match cmnd with
            | Goto line -> Goto (findAddress line)
            | If(expr, line) -> If(expr, findAddress line)
            | _ -> cmnd in

        Seq.map patch assembly;;

    /// Executes a BASIC program with a starting environment.
    let exec program env =
        let rec loop proc =
            let ip = proc.InstructionPointer in
            if isValidTarget ip proc then
                let instr = Seq.item ip proc.ProgramMemory in
                step instr proc |> loop
            else // halt
                () in

        loop { InstructionPointer = 0;
               ProgramMemory = assemble program;
               DataMemory = env; };;

    /// Runs interpreter with a program already in memory, returns it updated.
    let run program =
        let printProgram (Program(prog)) =
            printf "\n";
            Map.toSeq prog // seq comes out already sorted
            |> Seq.iter (Line >> Parser.showLine >> (printfn "%s"));
            printf "\n" in

        let help = "\
System directives:
  HELP            Print this help message
  END             Shutdown BASIC system
  CLEAR           Clear the terminal
  LIST            Show program contents
  RUN             Execute program in memory
  <num>  <cmnd>   Insert command <cmnd> into line <num> of program

Supported BASIC commands:
  REM <remark>            Ignores comment <remark>
  PRINT <expr>            Print the result of evaluating <expr>
  LET <var> = <expr>      Set variable <var> to the value of <expr>
  INPUT <var>             Read user input (numeric) into variable <var>
  GOTO <num>              Jump to instruction <num>
  IF <expr> THEN <num>    Branch to instruction <num> if <expr> is true

Additional:
  Pressing Ctrl+D keys has the same effect of the END directive\n" in

        let rec loop prog =
            let read = printf "> "; System.Console.ReadLine() in
            match if isNull read then Ok End else Parser.parse read with
            | Ok Noop -> loop prog
            | Ok Help -> printfn "%s" help ; loop prog
            | Ok End -> prog
            | Ok Clear -> System.Console.Clear(); loop prog
            | Ok List -> printProgram prog; loop prog
            | Ok Run ->
               (try exec prog emptyEnvironment
                with RuntimeException(e, _) -> printfn "!! Runtime error: %s" e);
                loop prog
            | Ok (Code line) -> loop (insert line prog)
            | Error msg -> printfn "!! Syntax error: %s" msg; loop prog in

        printfn ":: Mini-BASIC version 0.1.53 by baioc\n"; loop program;;

end;;
