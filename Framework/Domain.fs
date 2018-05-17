[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> P( [[Q*VAR -> Q] U Q] )
*)


type Record1 = {
    Q1 : Node ;
    Var1 : Var ;
}


type Union1 = 
    | Map1 of Map<Record1, Node> 
    | Q2 of Node 


type Powerset1 = Union1 Set

type sigma = Powerset1

type AnalysisResult = Map<Node, sigma>
let subsetOP x = subset_pw x 
let supersetOP x = superset_pw x 
let unionOP x = union_pw x 
let intersectOP x = intersect_pw x 
