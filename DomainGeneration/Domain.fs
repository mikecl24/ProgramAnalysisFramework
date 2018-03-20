module Domain

// Generated Code Section: Domain type
(*
Q -> VAR -> P({Plus;Minus;Zero})
*)

type Node = Node of int
type Var = Var of string

type List1 =
    | Plus
    | Minus
    | Zero


type Powerset1 = Powerset1 of List1 Set

type Map1 = Map1 of Map<Var,Powerset1>

type AnalysisResult = AnalysisResult of Map<Node,Map1>