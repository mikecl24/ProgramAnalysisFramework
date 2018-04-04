module Domain

// Generated Code Section: Domain type
(*
Q -> P(Q*[Q*Q U VAR])
*)

type Node = Node of int
type Var = Var of string


type Record2 = {
    Q2 : Node ;
    Q3 : Node ;
}

type Union1 =
    | Record2 of Record2
    | Var1 of Node 


type Record1 = {
    Q1 : Node ;
    Union1 : Union1;
}

type Powerset1 = Record1 Set

type AnalysisResult = Map<Node,Powerset1>