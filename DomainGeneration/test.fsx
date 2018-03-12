open System
open System.Collections.Generic

(*
type maptest =  Dictionary<int, int>

let x = maptest(Dictionary<int, int>())
x.Add(1, 2)
let y = maptest(Dictionary<int, int>())
y.Add(1, 3)
printfn "%A" x
printfn "%A" y
*)


// This gives: seq<int * int> -> Map<int,int>
type Maptest = Maptest of Map<int, int>


let x = Map.empty
let z1 = Maptest(x)
let z2 = Maptest(x.Add(1,1))
let y = x.Add(1,1).Add(1,2)

printfn "%A" z1
//let y = maptest(x)
//let y = maptest.empty
