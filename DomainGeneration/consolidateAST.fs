module consolidateAST

let getValueCLS (CartesianListSet v) = v
let getValueCLD (CartesianListDom v) = v

let rec getLast (array : string []) = 
    array.[array.Length - 1]

let x = CartesianDom  (PowersetDom QSet, PowersetDom (CartesianSet (VARSet, UnionSet (QSet, ListSet (Element "QM")))))

let rec toCartList ast =  
    match ast with
    | Element(fElem)                        -> Element(fElem) 
    | LargerList(fElem, fList)              -> LargerList(fElem, toCartList fList)


let rec toCartSet ast =  
    match ast with
    | CartesianSet(fSet1, fSet2)            -> match fSet1 with
                                                | CartesianSet(f1, f2)  -> CartesianListSet( getValueCLS (toCartSet fSet1)  @ [toCartSet fSet2] )
                                                | _                     -> CartesianListSet([toCartSet fSet1] @ [toCartSet fSet2]) 
    | QSet                                  -> QSet
    | VARSet                                -> VARSet
    | UnionSet(fSet1, fSet2)                -> UnionSet(toCartSet fSet1, toCartSet fSet2) 
    | ListSet(fList)                        -> ListSet(toCartList fList)
    | _                                     -> failwith "Already mutated AST"

let rec reduceDom ast = 
    match ast with
    | PowersetDom(fSet)                     -> PowersetDom(toCartSet fSet)
    | TotalFunctionSpaceDom(fSet, fDom)     -> TotalFunctionSpaceDom(toCartSet fSet, reduceDom fDom)
    | CartesianDom(fDom1, fDom2)            ->  match fDom1 with
                                                | CartesianDom(f1, f2)  -> CartesianListDom( getValueCLD (reduceDom fDom1)  @ [reduceDom fDom2] )
                                                | _                     -> CartesianListDom([reduceDom fDom1] @ [reduceDom fDom2]) 
    | _                                     -> failwith "Already mutated AST"
