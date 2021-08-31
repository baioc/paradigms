namespace Shared

type BinOp =
    | Add
    | Subtract
    | Multiply
    | Divide

type Result =
    { operation: BinOp
      lhs: float
      rhs: float
      result: float }

type Operators = { lhs: float; rhs: float }

type calculator =
    { addition: Operators -> Async<Result>
      subtraction: Operators -> Async<Result>
      multiplication: Operators -> Async<Result>
      division: Operators -> Async<Result> }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
