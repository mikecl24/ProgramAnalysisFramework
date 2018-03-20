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

#load "DomainParser.fs"
open DomainParser                   // Domain String -> Domain AST

#load "consolidateAST.fs"
open consolidateAST                 // Domain AST -> Flattened Domain AST

#load "DomainGenerator.fs"
open DomainGenerator                // Domain AST -> Code

//let DomainString = "P( VAR * [Q U {QM}] * Q )"
//let DomainString = "P(VAR * {Minus; Plus; Zero})"
let DomainString = "VAR -> P({Plus;Minus;Zero})"
//let DomainString = "P( Q )"

// Domain String -> Domain AST
let domainAST = ParseString (DomainString)

let flatDomainAST = reduceDom (domainAST)
printfn "%s" (flatDomainAST.ToString())

let code = evaluateAST flatDomainAST DomainString
printfn "%s" code

File.WriteAllText("Domain.fs", code)