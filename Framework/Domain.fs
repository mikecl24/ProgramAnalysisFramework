[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> [ [VAR U ARR] -> P({Plus; Zero; Minus}) ]
*)


type List1 =
    | Plus
    | Zero
    | Minus


type Powerset1 = List1 Set

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
