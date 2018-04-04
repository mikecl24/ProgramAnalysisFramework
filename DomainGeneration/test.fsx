open System
open System.Collections.Generic

type Node = Node of int
type Var = Var of string

type Powerset2 = Powerset2 of Node Set

type Powerset1 = Powerset1 of Node Set

type Record1 = {
    Powerset1 : Powerset1;
    Powerset2 : Powerset2;
}

let x = {Powerset1 = Powerset1(Set.empty.Add(Node(1))); Powerset2 = Powerset2(Set.empty.Add(Node(2)))}
//printfn "%A" x

type tutest = int * int

let y = tutest(1, 2)
printfn "%A" (y.GetType())