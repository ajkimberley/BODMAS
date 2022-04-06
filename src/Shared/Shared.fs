namespace Shared

open System

type LeafNode = { Token: string }

type UnaryOperatorNode = { Token: string; Operand: Node }

and BinaryOperatorNode = { Token: string; LHS: Node; RHS: Node }

and Node =
    | LeafNode of LeafNode
    | BinaryOperatorNode of BinaryOperatorNode

type Associativity =
    | Left
    | Right
    | None

type TokenType =
    | Number
    | AdditionOperator
    | SubtractionOperator
    | MultiplicationOperator
    | DivisionOperation
    | ExponentiationOperator
    | LeftBracket
    | RightBracket

type Token =
    { Type: TokenType
      Associativity: Associativity
      PrecedenceLevel: int
      Element: string }

module Token =
    let createNumber input =
        { Type = Number
          Associativity = None
          PrecedenceLevel = 0
          Element = input }

    let createAdditionOperator () =
        { Type = AdditionOperator
          Associativity = Left
          PrecedenceLevel = 1
          Element = "+" }

    let createSubtractionOperator () =
        { Type = SubtractionOperator
          Associativity = Left
          PrecedenceLevel = 1
          Element = "-" }

    let createMultiplicationOperator () =
        { Type = MultiplicationOperator
          Associativity = Left
          PrecedenceLevel = 2
          Element = "*" }

    let createDivisionOperator () =
        { Type = DivisionOperation
          Associativity = Left
          PrecedenceLevel = 2
          Element = "/" }

    let createExponentiationOperator () =
        { Type = ExponentiationOperator
          Associativity = Right
          PrecedenceLevel = 3
          Element = "^" }

    let createLeftBracket () =
        { Type = LeftBracket
          Associativity = None
          PrecedenceLevel = 0
          Element = "(" }

    let createRightBracket () =
        { Type = RightBracket
          Associativity = None
          PrecedenceLevel = 0
          Element = ")" }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IBodmasApi =
    { processInput: string -> Async<Result<Node,string>> }
