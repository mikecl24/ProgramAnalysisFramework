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

let programString = "
skip;
x[1]:=10
"
// Program String -> Statement List
let stmtList = ParseString programString

// Statement List -> Program Graph 
let Edges = GraphStatements stmtList



// Nodes: This should come from the graph generation (Hint: range(0,largest nr found))
let Nodes = [0; 1; 2]

// Variables: This should come from the parser
let Variables = ["x"]

// MetaL Parser -> Generate Domain
// Quickchecking?

#load "Domain.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types

// Pause
// TF class generation

#load "TransferFunctions.fs"        // Transfer Function Specification
open TransferFunctions              // iota, init

// Completeness + Correctness Check
// QuickChecking Module

#load "Analysis.fs"                 // Analysis Implementation:
open Analysis                       // Program Graph -> Analysis Result



// Program Graph -> Analysis Result
let Analysis = AnalyseEdges Edges

printfn "Result"
printfn "%A" (Seq.toList Analysis)
//printfn "%A" Analysis.[3]