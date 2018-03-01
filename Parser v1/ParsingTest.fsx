// Parser testing module:
// 

#r "FsLexYacc.Runtime.dll"

open Microsoft.FSharp.Text.Lexing
open System

#load "MicroCParser.fs"
open MicroCParser

#load "MicroCLexer.fs"
open MicroCLexer


type statement =
    { text : string;
    typeNr : int}

let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let result = MicroCParser.start MicroCLexer.tokenize lexbuf
    result


let programString = "
A[1]:=5-3;
A[x]:=A[1];
A[A[1]+A[2]]:= A[3]
"

try
let ret = parse programString //(Console.ReadLine())
printfn "%A" ret
with e -> failwith "Error parsing program"