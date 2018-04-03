module Domain

// Generated Code Section: Domain type
(*
Q -> Q -> P(VAR * Q) * VAR -> P(Q)
*)

type Node = Node of int
type Var = Var of string


type Powerset2 = Node Set

type Map2 = Map<Var,Powerset2>

type Record2 = {
    Var1 : Var ;
    Q1 : Node ;
}

type Powerset1 = Record2 Set

type Map1 = Map<Node,Powerset1>

type Record1 = {
    Map1 : Map1;
    Map2 : Map2;
}

type AnalysisResult = Map<Node,Record1>