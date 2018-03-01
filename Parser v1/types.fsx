open System
open System.Collections.Generic

let x = Set.empty.Add(1).Add(2).Add(3)

let y = Set.empty.Add(3).Add(4)

let z = Set.union x y

let res = Set.isSubset y z
if res then printfn "nice"