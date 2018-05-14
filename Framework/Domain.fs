[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> [VAR U ARR] -> P([Q * Q U {QM}])
*)


type Record1 = {
    Q1 : Node ;
    Q2 : Node ;
}

type List1 =
    | QM


type Union2 = 
    | Record1 of Record1
    | List1 of List1


type Powerset1 = Union2 Set

type Union1 = 
    | Var1 of Var 
    | Arr1 of Arr 


type Map1 = Map<Union1,Powerset1>

type sigma = Map1

type AnalysisResult = Map<Node, sigma>
let subsetOP x = subset_m (subset_pw) x 
let supersetOP x = superset_m (superset_pw) x 
let unionOP x = union_m (union_pw) x 
let intersectOP x = intersect_m (intersect_pw) x 
