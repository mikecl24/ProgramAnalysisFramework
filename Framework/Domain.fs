[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> P(IDENT * [Q U {QM}] * Q)
*)


type List1 =
    | QM


type Union1 =
    | Node1 of Node 
    | List1 of List1


type Record1 = {
    Ident1 : Ident ;
    Union1 : Union1;
    Node2 : Node ;
}

type Powerset1 = Record1 Set

type sigma = Powerset1

type AnalysisResult = Map<Node, sigma>
