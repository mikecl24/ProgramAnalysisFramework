[<AutoOpen>]
module DomainParser
open System

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let result = MetaLParser.start MetaLLexer.tokenize lexbuf
    result

let ParseStringDom inputString =
    try
    parse inputString 
    with e -> failwith "Error parsing domain: Invalid domain input"