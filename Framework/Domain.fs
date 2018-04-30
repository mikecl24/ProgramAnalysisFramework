module Domain

// Generated Code Section: Domain type
(*
Q -> P( VAR * [Q U {QM}] * Q )
*)

type List1 =
    | QM


type Union1 =
    | Q1 of Node 
    | List1 of List1


type Record1 = {
    Var1 : Var ;
    Union1 : Union1;
    Q2 : Node ;
}

type Powerset1 = Record1 Set

type AnalysisResult = Map<Node,Powerset1>

let subsetOP = subset_pw
let supersetOP = superset_pw
let unionOP = union_pw
let intersectOP = intersect_pw