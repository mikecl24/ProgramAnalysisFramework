module Domain

// Generated Code Section: Domain type
(*
Q -> P( VAR * [Q U {QM}] * Q )
*)

type Node = Node of int
type Var = Var of string

type List1 =
    | QM


type Union1 =
    | Q1 of Node 
    | List1 of List1


type Record1 = {
    VAR1 : Var ;
    Union1 : Union1;
    Q2 : Node ;
}

type Powerset1 = Record1 Set

type AnalysisResult = AnalysisResult of Map<Node,Powerset1>
 Hello