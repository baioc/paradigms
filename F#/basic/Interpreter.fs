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


module Interpreter = begin

    let private fixCase = String.map System.Char.ToUpper;;


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
            // string x int
            | Ok (String a), Plus, Ok (Int b) -> Ok (String (a + (string b)))
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
          VariableMemory: Environment;
          ReturnStack: int list };;

    /// Checks if an instruction address is valid inside a process.
    let internal isValidTarget addr proc =
        0 <= addr && addr < Seq.length proc.ProgramMemory;;

    exception internal RuntimeException of string * Process;;

    /// Executes a single command and steps the process one instruction forward.
    let internal step instr proc =
        let halt proc = { proc with InstructionPointer = -1 } in

        let fetchNext proc =
            let next = proc.InstructionPointer + 1 in
            if isValidTarget next proc
            then { proc with InstructionPointer = next }
            else halt proc in

        let set var value proc =
            { proc with VariableMemory = define var value proc.VariableMemory } in

        let jump addr proc = { proc with InstructionPointer = addr } in

        let call addr proc =
            { proc with
                InstructionPointer = addr;
                ReturnStack = proc.InstructionPointer :: proc.ReturnStack } in

        let eval expr = eval expr proc.VariableMemory in

        let runtimeError = raise << RuntimeException in

        match instr with
        | Remark _ -> fetchNext proc
        | Print expr ->
            ( match eval expr with
              | Ok value ->
                    sprintf "%O" value
                    |> fixCase
                    |> printfn "%s";
                    fetchNext proc
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
            ( match eval expr with
              | Ok (Bool false) -> fetchNext proc
              | Ok (Bool true) ->
                  if isValidTarget addr proc
                  then jump addr proc
                  else runtimeError("non-existent IF target address", proc)
              | Ok v ->
                  runtimeError(sprintf "wrong type %A at IF condition" v, proc)
              | Error msg -> runtimeError(msg, proc) )
        | Gosub callee ->
            if isValidTarget callee proc
            then call callee proc
            else runtimeError("non-existent GOSUB target address", proc)
        | Return ->
            match proc.ReturnStack with
            | [] -> halt proc
            | caller :: stack ->
                fetchNext { proc with InstructionPointer = caller;
                                      ReturnStack = stack };;

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
            | Gosub line -> Gosub (findAddress line)
            | _ -> cmnd in

        Seq.map patch assembly;;

    /// Executes a BASIC program with a starting environment.
    let exec program =
        let rec loop proc =
            let ip = proc.InstructionPointer in
            if isValidTarget ip proc then
                let instr = Seq.item ip proc.ProgramMemory in
                loop (step instr proc)
            else // halt
                () in

        loop { InstructionPointer = 0;
               ProgramMemory = assemble program;
               VariableMemory = emptyEnvironment;
               ReturnStack = []; };;

    /// Parses and loads a program from a file with BASIC code.
    let load file =
        let parseLine line =
            match Parser.parse line with
            | Ok (Code line) -> Some line
            | Error msg ->
                sprintf "!! Syntax error: %s" msg
                |> fixCase
                |> printfn "%s";
                None
            | Ok Noop -> None
            | directive ->
                sprintf "!! Syntax error: directive \"%A\" in program" directive
                |> fixCase
                |> printfn "%s";
                None in

        System.IO.File.ReadLines file
        |> Seq.choose parseLine
        |> Seq.fold (fun prog -> fun line -> insert line prog) idleProgram;;

    /// Runs interpreter with a program already in memory, returns it updated.
    let run program =
        let help = fixCase "\
System directives:
  HELP            Print this help message.
  END             Shutdown BASIC system.
  CLEAR           Clear the terminal.
  LIST            Show program contents.
  LOAD <path>     Loads a BASIC program from file <path> into memory.
  SAVE <path>     Saves the program in memory to file <path>.
  NEW             Delete the current program.
  RUN             Execute program in memory.
  <num>  <cmnd>   Insert command <cmnd> into line <num> of program.

Supported BASIC commands:
  REM <remark>            Ignores comment <remark>.
  PRINT <expr>            Print the result of evaluating <expr>.
  LET <var> = <expr>      Set variable <var> to the value of <expr>.
  INPUT <var>             Read user input (numeric) into variable <var>.
  GOTO <num>              Jump to instruction <num>.
  IF <expr> THEN <num>    Branch to instruction <num> if <expr> is true.
  GOSUB <num>             Go execute the subroutine at line <num>.
  RETURN                  Return from a subroutine to its caller, or end the
                          program when used from the main routine.

Additional:
  Pressing Ctrl+D keys has the same effect of the END directive.
" in

        let printProgram (Program(prog)) file =
            Map.toSeq prog // seq comes out already sorted
            |> Seq.map (Line >> Parser.showLine >> fixCase)
            |> Seq.iter (fprintfn file "%s") in

        let rec loop prog =
            let read = printf "> "; System.Console.ReadLine() in
            match if isNull read then Ok End else Parser.parse (fixCase read) with
            | Ok Noop -> loop prog
            | Ok Help -> printfn "%s" help; loop prog
            | Ok End -> prog
            | Ok Clear -> System.Console.Clear(); loop prog
            | Ok List ->
                printf "\n";
                printProgram prog stdout;
                printf "\n";
                loop prog
            | Ok (Load path) -> loop (load path)
            | Ok (Save path) ->
                using (System.IO.File.CreateText(path)) (printProgram prog);
                loop prog
            | Ok New -> loop idleProgram
            | Ok Run ->
                (try
                    exec prog
                 with
                 | RuntimeException(e, _) ->
                    sprintf "!! Runtime error: %s" e
                    |> fixCase
                    |> printfn "%s" );
                loop prog
            | Ok (Code line) -> loop (insert line prog)
            | Error msg ->
                sprintf "!! Syntax error: %s" msg
                |> fixCase
                |> printfn "%s";
                loop prog in

        fixCase ":: Mini-BASIC version 0.1.53 by baioc (tip: turn Caps Lock ON)\n"
        |> printfn "%s";
        loop program;;

end;;
