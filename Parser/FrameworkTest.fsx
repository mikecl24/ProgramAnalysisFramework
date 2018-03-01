#r "FsLexYacc.Runtime.dll"          // Load in FsLexYacc dll

open Microsoft.FSharp.Text.Lexing   // Lexing Library
open System
open System.Collections.Generic

#load "Types.fs"
open Types                          // Module with all the types

#load "MicroCParser.fs"
open MicroCParser                   // Generated Parser

#load "MicroCLexer.fs"
open MicroCLexer                    //Generated Lexer

#load "Parser.fs"                   // Parser Implementation:
open Parser                         // Program String -> Statement List

#load "Grapher.fs"                  // Graph Generator:
open Grapher                        // Statement List -> Program Graph 

#load "Analysis.fs"
open Analysis

let programString = "
skip;
x[1]:=10
"
// Program String -> Statement List
let stmtList = ParseString programString

// Statement List -> Program Graph 
let Edges = GraphStatements stmtList

let Analysis = AnalyseEdges Edges

printfn "Result"
printfn "%A" (Seq.toList Analysis)
//printfn "%A" Analysis.[3]
