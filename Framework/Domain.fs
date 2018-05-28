[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> P(VAR * [Q U {QM1}] * Q) * P(ARR * [Q U {QM2}] * Q)
*)


type List2 =
    | QM2


type Union2 =
    | Node3 of Node 
    | List2 of List2


type Record2 = {
    Arr2 : Arr ;
    Union2 : Union2;
    Node4 : Node ;
}

type Powerset2 = Record2 Set

type List1 =
    | QM1


type Union1 =
    | Node1 of Node 
    | List1 of List1


type Record1 = {
    Var2 : Var ;
    Union1 : Union1;
    Node2 : Node ;
}

type Powerset1 = Record1 Set

type ComplexDomain = Powerset1 * Powerset2 

type sigma = ComplexDomain

type AnalysisResult = Map<Node, sigma>
