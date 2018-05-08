#r "FsLexYacc.Runtime.dll"          // Load in FsLexYacc dll

open Microsoft.FSharp.Text.Lexing   // Lexing Library
open System
open System.IO
open System.Collections.Generic

#load "DomainTypes.fs"
open DomainTypes                    // Types: AST

#load "MetaLParser.fs"
open MetaLParser                    // Generated Parser

#load "MetaLLexer.fs"
open MetaLLexer                     //Generated Lexer

#load "DomainParser.fs"             // Domain String -> Domain AST
#load "consolidateAST.fs"           // Domain AST -> Flattened Domain AST
#load "DomainGenerator.fs"          // Domain AST -> Code
#load "CallGenerator.fs"            // Domain AST -> LattOps code

let DomainString = File.ReadAllText("Domain.metaL")

// Domain String -> Domain AST
let domainAST = ParseString (DomainString)

let flatDomainAST:domain = reduceDom (domainAST)
printfn "%s" (flatDomainAST.ToString())

let code = evaluateAST flatDomainAST DomainString
printfn "%s" code

let lattOps = evaluateASTCalls flatDomainAST
printfn "%s" lattOps

File.WriteAllText("Domain.fs", code + lattOps)