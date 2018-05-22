(*-----------------------------------------------------------------*)
(*-----------------------------IMPORTS-----------------------------*)
(*-----------------------------------------------------------------*)

let debug = false

#r "FsLexYacc.Runtime.dll"          // Load in FsLexYacc dll

//open System
open Microsoft.FSharp.Text.Lexing   // Lexing Library
open System.IO                      // File Handling

#load "Types.fs"                    // Module with types

#load "ExtWParser.fs"
open ExtWParser                     // Generated Parser from .fsp file

#load "ExtWLexer.fs"
open ExtWLexer                      //Generated Lexer from .fsl file

#load "Parser.fs"                   // Program String -> Statement List
#load "Grapher.fs"                  // Statement List -> Program Graph 

(*-----------------------------------------------------------------*)
(*-------------------CODE-PRE-PROCESSING-MODULE--------------------*)
(*-----------------------------------------------------------------*)

(* File -> Program String *)
let programString : string = File.ReadAllText("Program.extw")

(* Program String -> Statement List *)
let stmtList : Statement list = ParseString programString

(* Statement List -> Program Graph *)
let Edges : Edge list = GraphStatements stmtList
printfn "Program Graph:"
//printfn "%A" Edges


(* Node list generated by Grapher *)
let Nodes : Node list = ExtractNodes Edges
// printfn "%A" Nodes

(* Variable list from Grapher *)
let Variables : Var list = uniqueVars vartemp
let Arrays : Arr list = uniqueVars arrtemp
// printfn "%A" Variables
// printfn "%A" Arrays

let Identifiers : Identifier list =
 List.map (fun x -> Var1 x) Variables @
 List.map (fun x -> Arr1 x) Arrays

printfn "Code Pre-processing Done"

(*-----------------------------------------------------------------*)
(*------------------------EXT-DOMAIN-CHECK-------------------------*)
(*-----------------------------------------------------------------*)

// MetaL Parser -> Generate Domain
// Quickchecking domain?

(*-----------------------------------------------------------------*)
(*--------------------------DOMAIN-MODULE--------------------------*)
(*-----------------------------------------------------------------*)

(*                             imports                             *)

#load "DomainTypes.fs"              // Types: AST

#load "MetaLParser.fs"
open MetaLParser                    // Generated Parser
#load "MetaLLexer.fs"
open MetaLLexer                     //Generated Lexer

#load "DomainParser.fs"             // Domain String -> Domain AST
#load "DomainGenerator.fs"          // Domain AST -> Code
#load "CallGenerator.fs"            // Domain AST -> LattOps code
#load "consolidateAST.fs"           // Domain AST -> Flattened Domain AST

// File -> String
let DomainString = File.ReadAllText("Domain.metaL")

// String -> Domain AST
let domainAST = ParseStringDom (DomainString)
printfn "%A" (domainAST)

generateCode (domainAST, DomainString)

#load "LattOps.fs"                      // Lattice Opreations: Union, Intersection, Subset, Superset
#load "Domain.fs"                       // Domain Specification: Generated code and call traces for LattOps

printfn "Domain Generation Done"

(*-----------------------------------------------------------------*)
(*---------------------TRANSFER-FUNCTION-MODULE--------------------*)
(*-----------------------------------------------------------------*)

#load "TransferFunctions.fs"        // Transfer Function Specification + 
                                    // Direction, combination op, iota, init

(*                          QuickChecking                          *)
//insert QuickChecking folder stuff here

printfn "Transfer Function Done"

(*-----------------------------------------------------------------*)
(*-------------------------ANALYSIS-MODULE-------------------------*)
(*-----------------------------------------------------------------*)

#load "Analysis.fs"                 // Program Graph -> Analysis Result

// Program Graph -> Analysis Result
let Analysis : AnalysisResult = AnalyseEdges Edges // union/intersection function, sub/super -seteq function as parameters


(*-----------------------------------------------------------------*)
(*-----------------------------RESULTS-----------------------------*)
(*-----------------------------------------------------------------*)

printfn "Analysis Done\n\n"
Analysis |> Seq.iter (fun x -> printfn "%A\t ->" x.Key; printfn "%A" (x.Value); printfn "\n")