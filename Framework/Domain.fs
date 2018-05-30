[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> [ IDENT -> [ [ {Bot} U [INT U {NegInf}] * [INT U {PlusInf}] ] ] ]
*)


type List1 =
    | Bot


type List2 =
    | NegInf


type Union2 =
    | Int1 of int 
    | List2 of List2


type List3 =
    | PlusInf


type Union3 =
    | Int2 of int 
    | List3 of List3


type Record1 = {
    Union2 : Union2;
    Union3 : Union3;
}

type Union1 = 
    | List1 of List1
    | Record1 of Record1


type Map1 = Map<Ident,Union1>

type sigma = Map1

type AnalysisResult = Map<Node, sigma>
