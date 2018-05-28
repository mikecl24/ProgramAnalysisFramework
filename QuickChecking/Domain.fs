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


type Map1 = Map<Identifier, Powerset1>

type sigma = Map1

type AnalysisResult = Map<Node, sigma>