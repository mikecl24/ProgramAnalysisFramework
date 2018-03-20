open System
open System.Collections.Generic

type Record1 = {
    VAR1 : string ;
}

type Powerset1 = Powerset1 of Record1 Set
type AnalysisResult = AnalysisResult of Map<int,Powerset1>

let result = AnalysisResult( Map.empty.Add( 0, Powerset1( Set.empty.Add( {VAR1 = "testing"} ) ) ) )
printfn "%A" result