open System
open System.Collections.Generic

#load "Domain.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types

let getValueAR (AnalysisResult v) = v

let updateAnalysisResult ((p:AnalysisResult), (q:AnalysisResult)) = 
    Map(Seq.concat [ (Map.toSeq (getValueAR p)) ; (Map.toSeq (getValueAR q)) ])

let result = AnalysisResult(Map.empty.Add(Node(1),Powerset1(Set.empty.Add( {VAR1 = Var("test"); Union1 = Q1(Node(1));Q2 = Node(2)} ))))
let res2 = updateAnalysisResult (result, AnalysisResult(Map.empty.Add(Node(1),Powerset1(Set.empty.Add( {VAR1 = Var("test2"); Union1 = Q1(Node(2));Q2 = Node(1)} )))))
printfn "%A" res2

let y = [1; 2; 3]
printfn "%i" ( y.[y.Length-1] )
