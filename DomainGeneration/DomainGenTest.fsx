#r "FsLexYacc.Runtime.dll"          // Load in FsLexYacc dll

open Microsoft.FSharp.Text.Lexing   // Lexing Library
open System
open System.Collections.Generic

#load "DomainTypes.fs"
open DomainTypes                    // Types: AST

#load "MetaLParser.fs"
open MetaLParser                    // Generated Parser

#load "MetaLLexer.fs"
open MetaLLexer                     //Generated Lexer

#load "DomainParser.fs"
open DomainParser                   // Domain String -> Domain AST

#load "DomainGenerator.fs"
open DomainGenerator                // Domain AST -> Code

let DomainString = "P( VAR * [Q U {QM}] * Q )"

// Domain String -> Domain AST
let domainAST = ParseString (DomainString)
printfn "%s" (domainAST.ToString())

let code = evaluateAST domainAST DomainString
printfn "%s" code