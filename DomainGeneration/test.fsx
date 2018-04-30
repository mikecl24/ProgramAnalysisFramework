open System

type testt = int * int

let x = "test!!"
printfn "%s" (x.Remove(x.Length-2))