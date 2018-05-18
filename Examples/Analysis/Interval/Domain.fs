[<AutoOpen>]
module Domain

type Node = Node of int
type Var = Var of string
type Arr = Arr of string

type List3 = 
    | PlusInf

type Union4 = 
    | Int2 of int
    | List3 of List3

type List2 = 
    | NegInf

type Union3 = 
    | Int1 of int
    | List2 of List2

type Record1 = {
    Union3 : Union3;
    Union4 : Union4;
}

type List1 =
    | Bot

type Union2 =
    | Record1 of Record1
    | List1 of List1

type Union1 =
    | Var1 of Var
    | Arr1 of Arr

type Map1 = Map<Union1, Union2>

type sigma = Map1

type AnalysisResult = Map<Node, sigma>

(*   Provided Graph Latt ops!   *)
let subset_s (x:Record1,y:Record1) : bool =     (y.Union3 = List2 NegInf || ((not (x.Union3 = List2 NegInf)) && (y.Union3 <= x.Union3))) &&
                                                    (y.Union4 = List3 PlusInf || ((not (x.Union4 = List3 PlusInf)) && (y.Union4 >= x.Union4)))


let superset_s (x:Record1,y:Record1) : bool = (x.Union3 = List2 NegInf || ((not (y.Union3 = List2 NegInf)) && (x.Union3 <= y.Union3))) &&
                                                    (x.Union4 = List3 PlusInf || ((not (y.Union4 = List3 PlusInf)) && (x.Union4 >= y.Union4)))

let union_u3 (x:Union3,y:Union3) : Union3 =   if (x = List2 NegInf) || y = List2 NegInf 
                                                then List2 NegInf  
                                                elif (x<y) then x
                                                else y 
let union_u4 (x:Union4,y:Union4) : Union4 =   if (x = List3 PlusInf) || y = List3 PlusInf 
                                                then List3 PlusInf  
                                                elif (x>y) then x
                                                else y 
let union_s (x:Record1,y:Record1) : Record1 = {Union3 = (union_u3 (x.Union3, y.Union3)); Union4 = (union_u4 (x.Union4, y.Union4))}


let intersect_u3 (x:Union3, y:Union3) : Union3 =    if (x = List2 NegInf) && y = List2 NegInf then List2 NegInf
                                                    elif (x = List2 NegInf) then y
                                                    elif (y = List2 NegInf) then x
                                                    elif (x<y) then y
                                                    else x
let intersect_u4 (x:Union4,y:Union4) : Union4 =     if (x = List3 PlusInf) && y = List3 PlusInf then List3 PlusInf
                                                    elif (x = List3 PlusInf) then y
                                                    elif (y = List3 PlusInf) then x
                                                    elif (x>y) then y
                                                    else x

let intersect_s (x:Record1,y:Record1) : Record1 = {Union3 = (intersect_u3 (x.Union3, y.Union3)); Union4 = (intersect_u4 (x.Union4, y.Union4))}

(*
let subsetOP (x,y) = subset_m (subset_s) (x,y)
let supersetOP (x,y) = superset_m (superset_s) (x,y)
let unionOP (x,y) = union_m (union_s) (x,y)
let intersectOP (x,y) = intersect_m (intesect_s) (x,y)
*)