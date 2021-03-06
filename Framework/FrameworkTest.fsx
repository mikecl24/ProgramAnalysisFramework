(*-----------------------------------------------------------------*)
(*-----------------------------IMPORTS-----------------------------*)
(*-----------------------------------------------------------------*)
#r "FsLexYacc.Runtime.dll"          // Load in FsLexYacc dll
#r "FsCheck.dll"                    // Load in FsCheck.dll

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
// printfn "Program Graph:"
// printfn "%A" Edges


(* Node list generated by Grapher *)
let Nodes : Node list = ExtractNodes Edges
// printfn "%A" Nodes

(* Variable list from Grapher *)
let Variables : Var list = uniqueSeq vartemp
let Arrays : Arr list = uniqueSeq arrtemp
// printfn "%A" Variables
// printfn "%A" Arrays

let Identifiers : Ident list =
 List.map (fun x -> Var1 x) Variables @
 List.map (fun x -> Arr1 x) Arrays

printfn "Code Pre-processing Done"

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
#load "consolidateAST.fs"           // Domain AST -> Flattened Domain AST
#load "DomainGenerator.fs"          // Domain AST -> Code
#load "CallGenerator.fs"            // Domain AST -> LattOps code

// File -> String
let DomainString : string = File.ReadAllText("Domain.metaL")
//printfn "%A" DomainString

// String -> Domain AST
let domainAST : domain = ParseStringDom (DomainString)
//printfn "%A" domainAST

// Domain AST -> flattened Domain AST
let flatDomainAST : domain = reduceDom (domainAST)
// printfn "%A" flatDomainAST

// flattened Domain AST -> Types
let (code, mapNum, mapDescription) = evaluateAST flatDomainAST DomainString
//printfn "%s" code

// flattened Domain AST -> Lattice Operations
let lattOps = evaluateASTCalls flatDomainAST
//printfn "%s" lattOps

// Write results to files
File.WriteAllText("Domain.fs", code)
File.WriteAllText("Operations.fs", lattOps)

// Load geenrated code + standard ops
#load "LattOps.fs"                      // Lattice Opreations: Union, Intersection, Subset, Superset
#load "Domain.fs"                       // Domain Specification: Generated code

printfn "Domain Generation Done"

(*-----------------------------------------------------------------*)
(*--------------------------DOMAIN-WORK----------------------------*)
(*-----------------------------------------------------------------*)

// Verify Nature of domain: Safe or graph?
#load "DomainChecker.fs"                   // Check if domain is valid: return domain type, mapnr and mapdecriptor (last two for debugging)
let (isSafe, mNum, mapDescriptor) = domCheck flatDomainAST
// printfn "%A" (isSafe, mNum, mapDescriptor)    // Debugging correct mapdescriptor

// IF safe auto generate bot and top elements
open FsCheck
#load "GeneratorGenerator.fs"           // Create code to get top and bottom if it is a safe domain 

//printfn "%A\n%A\n%A\n%A" Nodes Variables Arrays Identifiers
// printfn "%i, %A" mapNum mapDescription   // Debugging: Parameters
let latticeCalculator = outputCode mapNum mapDescription
File.WriteAllText("LatticeStates.fs", latticeCalculator)

printfn "Calculating Bot and Top...    (may take some time)"
#load "LatticeStates.fs"                // Create bot and top

if isSafe then
    printfn "Generated bot: %A" bot
    printfn "Generated top: %A" top
else
    printfn "Graph Domain: Provide bot and top in TransferFuctions.fs"

printfn "Domain Verification Done"

(*-----------------------------------------------------------------*)
(*---------------------TRANSFER-FUNCTION-MODULE--------------------*)
(*-----------------------------------------------------------------*)

#load "TransferFunctions.fs"        // Transfer Function Specification + 
                                    // Direction, combination op, iota, init
                                    // NEED subset_s if unsafe!!!!!
                                    
#load "Operations.fs"                   // Operation Specification: Generated call traces for LattOps

// Quickchecking domain properties extension?

(*                          QuickChecking                          *)
// Generate QuickChecker executable
// Must be executed on the side:
//      Generators is a mutable in the library
//      Standard set generator was lost when using FsCheck to gen top/bot
//      No unregister/reset on generators, thus only doable by fresh run

#load "QCheckGenerator.fs"
let parameters = File.ReadAllText("QCheckParameters.txt")
File.WriteAllText("GenQuickChecker.fsx", (assemble parameters mapNum mapDescription))
printfn "Quickchecking verification program generated in GenQuickChecker.fsx (must be run separately)"

printfn "Transfer Function Loading Done"


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