



namespace Basic
  type Lexer =
    { Source: string
      Cursor: int }
    with
      member Length : int
    end
  type Token =
    | Natural of int
    | String of string
    | Word of string
    | Operator of string
    | LeftParenthesis
    | RightParenthesis
    | EndOfText
  module Lexer = begin
    val make : source:string -> Lexer
    val internal peek : lexer:Lexer -> char option
    val internal step : lexer:Lexer -> Lexer
    val internal extractWhile :
      pred:(char -> bool) -> lexer:Lexer -> Lexer * string
    val advance : lexer:Lexer -> Lexer * Result<Token,string>
    val tokenize : source:string -> seq<Token>
  end

namespace Basic
  type UnaryOperator =
    | Negative
    | Not
    with
      static member precedence : (UnaryOperator -> int)
    end
  type BinaryOperator =
    | Plus
    | Minus
    | Multiplication
    | Division
    | Modulo
    | Equal
    | Less
    | Greater
    | LessEqual
    | GreaterEqual
    | Different
    | And
    | Or
    with
      static member precedence : (BinaryOperator -> int)
    end
  type Expression =
    | Number of int
    | Variable of string
    | Text of string
    | Prefix of UnaryOperator * Expression
    | Infix of Expression * BinaryOperator * Expression
  type Command =
    | Remark of string
    | Goto of int
    | Let of string * Expression
    | Print of Expression
    | Input of string
    | If of Expression * int
    | Gosub of int
    | Return
  type Line = | Line of int * Command
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
    | Noop
  module Parser = begin
    val internal showExpression : (Expression -> string)
    val internal showCommand : _arg1:Command -> string
    val internal showLine : _arg1:Line -> string
    type private Operator =
      | Bin of BinaryOperator
      | Unr of UnaryOperator
      | Custom of string
    type private StackElement =
      | Expr of Expression
      | Op of Operator
      | LParen
      | Err of string
    type private Stack = | Stack of StackElement list
    exception private InvalidReductionException
    val private reduce : minPrec:int -> Stack -> Stack
    val private shiftReduce : lookahead:Token -> Stack -> Stack
    exception internal ParsingException of string
    val internal parseExpression :
      stopAt:(Token -> bool) -> lexer:Lexer -> Lexer * Expression
    val internal parseCommand : lexer:Lexer -> Command
    val parse : userInput:string -> Result<Directive,string>
  end

namespace Basic
  type Value =
    | Int of int
    | Bool of bool
    | String of string
    with
      override ToString : unit -> string
    end
  type Environment = | Environment of Map<string,Value>
  type Program = | Program of Map<int,Command>
  module Interpreter = begin
    val private fixCase : (string -> string)
    val emptyEnvironment : Environment
    val define : var:string -> value:Value -> Environment -> Environment
    val lookUp : var:string -> Environment -> Value option
    val eval : expr:Expression -> env:Environment -> Result<Value,string>
    val idleProgram : Program
    val insert : Line -> Program -> Program
    type internal Process =
      { InstructionPointer: int
        ProgramMemory: seq<Command>
        VariableMemory: Environment
        ReturnStack: int list }
    val internal isValidTarget : addr:int -> proc:Process -> bool
    exception internal RuntimeException of string * Process
    val internal step : instr:Command -> proc:Process -> Process
    val private assemble : Program -> seq<Command>
    val exec : program:Program -> unit
    val load : file:string -> Program
    val run : program:Program -> Program
  end

module Main
val main : argv:string [] -> int

