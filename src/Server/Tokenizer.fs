module BODMAS.Server.Tokenizer

open System
open BODMAS.Server.Types
open BODMAS.Server.Types.Token

let getToken (input: char list) : Result<Token, string> =
    match input with
    | head::[] when head = '+' -> createAdditionOperator () |> Ok
    | head::[] when head = '-' -> createSubtractionOperator () |> Ok
    | head::[] when head = '*' -> createMultiplicationOperator () |> Ok
    | head::[] when head = '/' -> createDivisionOperator () |> Ok
    | head::[] when head = '^' -> createExponentiationOperator () |> Ok
    | head::[] when head = '(' -> createLeftBracket () |> Ok
    | head::[] when head = ')' -> createRightBracket () |> Ok
    | cs when List.forall (fun c -> Char.IsDigit c) cs -> String.Concat(Array.ofSeq cs) |> createNumber |> Ok
    | _ -> $"Invalid input!" |> Error

let rec traverseResultM f list =
    let (>>=) x f = Result.bind f x
    let retn = Ok
    let cons head tail = head :: tail

    match list with
    | [] -> retn []
    | head :: tail ->
        f head
        >>= (fun h ->
            traverseResultM f tail
            >>= (fun t -> retn (cons h t)))

let group xs =
    let folder x = function
        | [] -> [[x]]
        | (h::t):: ta when Char.IsDigit x && Char.IsDigit h -> (x::h::t)::ta
        | acc -> [x]::acc
    Seq.foldBack folder xs []

let scan (input: string) =
    input
    |> group
    |> traverseResultM getToken