open System
open System.Collections.Generic
#load "DomainTypes.fs"
open DomainTypes                    // Types: AST

type Node = Node of int
type Var = Var of string

let z = Node(6)
printfn "%A" z

type recordSetType = {
    result : string
    r : int;
    cs : int;
    v : int;
    q : int;
    u : int;
    l : int;
    prepend : string
}

let x = {result = "?";
        r = 1;
        cs = 1;
        v = 1;
        q = 1;
        u = 1;
        l = 1;
        prepend = ""
        }

printfn "%s" (x.result.GetType().ToString())

(*
type maptest =  Dictionary<int, int>

let x = maptest(Dictionary<int, int>())
x.Add(1, 2)
let y = maptest(Dictionary<int, int>())
y.Add(1, 3)
printfn "%A" x
printfn "%A" y

// This gives: seq<int * int> -> Map<int,int>
type Maptest = Maptest of Map<int, int>


let x = Map.empty
let z1 = Maptest(x)
let z2 = Maptest(x.Add(1,1))
let y = x.Add(1,1).Add(1,2)

printfn "%A" z1
//let y = maptest(x)
//let y = maptest.empty


let x (y:int) : (int*int)= (y,2*y)

let y (a,b) = a

printfn "%A" (y (x 1))
*)
