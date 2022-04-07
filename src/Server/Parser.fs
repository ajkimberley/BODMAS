module BODMAS.Server.Parser

open Shared
open BODMAS.Server.Tokenizer

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
        let value = parseExpression 1 tail
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
            Ok lhs, tokens
        else
            let rhsResult = parseExpression (getNextMinLevel head) tail
            match rhsResult with
            | Ok rhs, remainder -> 
                let newLhs = BinaryOperatorNode { LHS = lhs; RHS = rhs; Token = head.Element }
                parseInner newLhs (minPrecLevel) remainder
            | Error err, remainder -> Error err, remainder
    | [] -> Ok lhs, [] 

and parseExpression (minPrecLevel: int) (tokens: Token list) : Result<Node, string> * Token list =
    match parseElement tokens minPrecLevel with
    | Ok lhs, [] -> Ok lhs, []
    | Ok lhs, remainder -> 
        
        parseInner lhs minPrecLevel remainder
    | Error err, remainder -> Error err, remainder

let parse (input: string) : Result<Node, string> =
    let scanResult = scan input

    match scanResult with
    | Ok tokens -> 
        let (parseResult, _) = parseExpression 1 tokens
        parseResult
    | Error err -> Error err