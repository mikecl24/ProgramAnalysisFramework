open System

let x = Map.empty.Add(1,2)
let y = x.Add(1,3)
printfn "%A" y