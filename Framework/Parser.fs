[<AutoOpen>]
module Parser
open System.Runtime.InteropServices

// Reorders the list of edges by creation order
let rec reverse (inList : 'a list) : 'a list=
    match inList with
    | []    ->  []
    | _     ->  List.rev inList

// Use the ExtW parser on a string
let ParseString (input : string) : Statement list =
    try
        let lexbuf = LexBuffer<char>.FromString input
        ExtWParser.start ExtWLexer.tokenize lexbuf 
    with e -> failwith "Error parsing program: Invalid code input"

// Convert a sequence to a list
let toList s = Set.fold (fun l se -> se::l) [] s

// Get the unique values in a sequence in the same order
let uniqueSeq (input : 'a seq) : 'a list = 
    reverse (toList (Set.ofSeq input))