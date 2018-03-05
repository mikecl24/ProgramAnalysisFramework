open System

type Node = 
 | N1
 | N2
 | N3
 | B_1a

let _mapTest1 = Map.empty.Add(N1, Set.empty.Add(1)).Add(N2,  Set.empty.Add(2))
let _mapTest2 = Map.empty.Add(N1,  Set.empty.Add(1)).Add(N2,  Set.empty.Add(2))


printfn "%A" _mapTest1

_mapTest1 |> Map.toList |> Set.ofList |> printfn "%A"

//let isSqSubset a b =