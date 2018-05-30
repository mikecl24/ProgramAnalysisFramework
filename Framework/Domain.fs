[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> [ VAR -> [ [{Bot; Top} U INT ] ] ]
*)


type List1 =
    | Bot
    | Top


type Union1 = 
    | List1 of List1
    | Int1 of int 


type Map1 = Map<Var,Union1>

type sigma = Map1

type AnalysisResult = Map<Node, sigma>
