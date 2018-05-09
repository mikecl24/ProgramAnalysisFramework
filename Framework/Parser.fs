[<AutoOpen>]
module Parser

// Reorders the list of edges by creation order
let rec reverse inList acc=
    match inList with
    | [] -> acc
    | [x] -> x::acc
    | head::tail -> reverse tail (head::acc)

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let result = ExtWParser.start ExtWLexer.tokenize lexbuf
    result

let ParseString inputString : Statement list =
    try
    parse inputString 
    with e -> failwith "Error parsing program: Invalid code input"

let toList s = Set.fold (fun l se -> se::l) [] s

let uniqueVars varmut = 
    reverse (toList (Set.ofSeq varmut)) []