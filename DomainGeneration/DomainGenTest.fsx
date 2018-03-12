#r "FsLexYacc.Runtime.dll"          // Load in FsLexYacc dll

open Microsoft.FSharp.Text.Lexing   // Lexing Library
open System
open System.Collections.Generic

#load "DomainParser.fs"
open DomainParser                   // Generated Parser

#load "DomainLexer.fs"
open DomainLexer                    //Generated Lexer

let DomainString = "
Q -> P( VAR * [Q U {QM}] * Q )
"

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let result = DomainParser.start DomainLexer.tokenize lexbuf
    result

let ParseString inputString =
    try
    parse inputString 
    with e -> failwith "Error parsing program: Invalid domain input"

// Domain String -> ?
let something = ParseString DomainString
printfn "%A" something
