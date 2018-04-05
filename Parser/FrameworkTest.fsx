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
if !x<>2 || true then
    y:=z-x+1
else 
    skip
fi"

// Program String -> Statement List
let stmtList = ParseString programString
let Variables = uniqueVars vartemp
printfn "%A" Variables

// Statement List -> Program Graph 
let Edges = GraphStatements stmtList
//printfn "%A" Edges

// Node list generated by Grapher 
let Nodes = ExtractNodes Edges



// MetaL Parser -> Generate Domain
// Quickchecking?

#load "Domain.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types

// Pause
// TF class generation

//#load "TransferFunctions2.fs"        // Transfer Function Specification
//open TransferFunctions              // iota, init

// Completeness + Correctness Check
// QuickChecking Module
(*
#load "Analysis2.fs"                 // Analysis Implementation:
open Analysis                       // Program Graph -> Analysis Result



// Program Graph -> Analysis Result
let Analysis = AnalyseEdges Edges // union/intersection function, sub/super -seteq function

printfn "Result"
printfn "%A" (Seq.toList (getValueAR Analysis))
//printfn "%A" Analysis.[3]
*)