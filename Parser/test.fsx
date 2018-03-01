open System

type elem1 = 
    | E_QM_

type testing = 
    {
        Num : int;
        Elem : elem1
    }

let x = { Num = 10; Elem = E_QM_ }
printfn "%A" x