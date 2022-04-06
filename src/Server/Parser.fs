module BODMAS.Server.Parser

open BODMAS.Server.Types
open BODMAS.Server.Tokenizer

let write (input: string) =
    System.Console.WriteLine(input)

let getNextMinLevel (token: Token) =
    match token.Associativity with
    | Left -> token.PrecedenceLevel + 1
    | _ -> token.PrecedenceLevel
    
let rec parseElement (tokens: Token list) (minPrecLevel: int) : Result<Node, string> * Token list =
    match tokens with
    | head :: tail when head.Type = Number ->
        let numNode = LeafNode { Token = head.Element }
        Ok numNode, tail
    | head :: tail when head.Type = LeftBracket ->
        write "found a bracket"
        let value = parseExpression minPrecLevel tail
        match value with
        | Ok value, head :: tail ->
            if (not (head.Type = RightBracket)) then Error "Unmatched bracket", tail
            else Ok value, tail
        | _, [] -> Error "Missing bracket", []
        | Error err, remainder -> Error err, remainder

    | _ -> Error "Empty input", tokens

and parseInner (lhs: Node) (minPrecLevel: int) (tokens: Token list) =
    match tokens with
    | head :: tail -> 
        if head.PrecedenceLevel < minPrecLevel 
        then 
            write "head prec was < than minPrec - escaping parseInner"
            Ok lhs, tokens
        else
            write "haed prec was >= minPrec - looping inner"
            let rhsResult = parseExpression (getNextMinLevel head) tail // (2, + 3)
            match rhsResult with
            | Ok rhs, remainder -> 
                let newLhs = BinaryOperatorNode { LHS = lhs; RHS = rhs; Token = head.Element }
                write $"looping back in parse inner with newLhs = {newLhs} and remainder = {remainder} and minPrecLevel = {minPrecLevel}"
                parseInner newLhs (minPrecLevel) remainder
            | Error err, remainder -> Error err, remainder
    | [] -> Ok lhs, [] 

and parseExpression (minPrecLevel: int) (tokens: Token list) : Result<Node, string> * Token list =
    match parseElement tokens minPrecLevel with
    | Ok lhs, [] -> Ok lhs, []
    | Ok lhs, remainder -> 
        
        parseInner lhs minPrecLevel remainder
    | Error err, remainder -> Error err, remainder

let parse (input: string) : Node =
    let scanResult = scan input

    match scanResult with
    | Ok tokens ->
        match parseExpression 1 tokens with
        | Ok result, _ -> result
        | Error err, _ -> failwith err
    | Error err -> failwith err