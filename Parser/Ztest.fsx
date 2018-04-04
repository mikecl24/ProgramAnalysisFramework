open System

let x = Set.empty.Add(1).Add(3).Add(4)
let y = Set.fold (fun l se -> se::l) [] x
printfn "%A" y