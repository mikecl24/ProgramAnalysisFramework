open System
open System.Collections.Generic

type Union1 = 
    | Q2 of int
    | U1_List1 of string // if it is "?"

// Record1 = Var1 * Union1 * Q2
type Record1 =
    {VAR1 : string;
    Union1 : Union1;
    Q1 : int
    }

let x = {VAR1 = "test"; Union1 = Q2(2); Q1 = 1}
printfn "%A" x 