module consolidateAST

let getValueCLS (CartesianListSet v) = v
let getValueCLD (CartesianListDom v) = v
let getValueEL (ElemList v) = v

let rec getLast (array : string []) = 
    array.[array.Length - 1]


let rec toCartList ast =  
    match ast with
    | Element(fElem)                        -> [ fElem ]
    | LargerList(fElem, fList)              -> match fList with
                                                | LargerList(fElem2, fList2)    -> [fElem] @ (toCartList fList)
                                                | Element(fElem3)               -> [ fElem ] @ [ fElem3 ] 


let rec toCartSet ast =  
    match ast with
    | CartesianSet(fSet1, fSet2)            -> match fSet1 with
                                                | CartesianSet(f1, f2)  -> CartesianListSet( getValueCLS (toCartSet fSet1)  @ [toCartSet fSet2] )
                                                | _                     -> CartesianListSet([toCartSet fSet1] @ [toCartSet fSet2]) 
    | QSet                                  -> QSet
    | VARSet                                -> VARSet
    | UnionSet(fSet1, fSet2)                -> UnionSet(toCartSet fSet1, toCartSet fSet2) 
    | ListSet(fList)                        -> ElemList(toCartList fList)
    | _                                     -> failwith "Already mutated AST"

let rec reduceDom ast = 
    match ast with
    | PowersetDom(fSet)                     -> PowersetDom(toCartSet fSet)
    | TotalFunctionSpaceDom(fSet, fDom)     -> TotalFunctionSpaceDom(toCartSet fSet, reduceDom fDom)
    | CartesianDom(fDom1, fDom2)            ->  match fDom1 with
                                                | CartesianDom(f1, f2)  -> CartesianListDom( getValueCLD (reduceDom fDom1)  @ [reduceDom fDom2] )
                                                | _                     -> CartesianListDom([reduceDom fDom1] @ [reduceDom fDom2]) 
    | _                                     -> failwith "Already mutated AST"