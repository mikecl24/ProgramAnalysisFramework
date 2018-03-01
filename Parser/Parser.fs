module Parser

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let result = MicroCParser.start MicroCLexer.tokenize lexbuf
    result

let ParseString inputString =
    try
    parse inputString 
    with e -> failwith "Error parsing program: Invalid code input"