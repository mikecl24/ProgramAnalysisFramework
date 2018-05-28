[<AutoOpen>]
module consolidateAST

// Extract Value in a cartesian list set: For list concatenation
let getValueCLS (v) : set list = 
    match v with
    | CartesianListSet v    ->  v
    | _                     ->  failwith "Extraction attempt with wrong funciton: CLS"

// Extract Value in a cartesian list domain: For list concatenation
let getValueCLD (v) : domain list = 
    match v with
    | CartesianListDom v    ->  v
    | _                     -> failwith "Extraction attempt with wrong funciton: CLD"

// Convert a ListSet to a string list for an ElemList object
let rec toListList (ast : elemlist) : string list =  
    match ast with
    | Element(fElem)                        -> [ fElem ]
    | LargerList(fElem, fList)              -> match fList with
                                                | Element(fElem2)       -> [ fElem ] @ [ fElem2 ] 
                                                | LargerList(_)         -> [fElem] @ (toListList fList)

// Flatten a set by merging same level cartesian sets and list sets
let rec toCartSet (ast : set) : set =  
    match ast with
    | SsetSet(_)                            -> ast
    | CartesianSet(fSet1, fSet2)            -> match fSet1 with
                                                | CartesianSet(_)   -> CartesianListSet( getValueCLS (toCartSet fSet1)  @ [toCartSet fSet2] )
                                                | _                 -> CartesianListSet([toCartSet fSet1] @ [toCartSet fSet2]) 
    | CartesianListSet(_)                   -> failwith "Error: Trying to mutate already mutated AST (Cartesian Set)"
    | TotalFunctionSpaceSet(fsset, fset)    -> TotalFunctionSpaceSet( fsset , toCartSet fset)
    | PowersetSet(fSet)                     -> PowersetSet( toCartSet fSet )
    | UnionSet(fSet1, fSet2)                -> UnionSet(toCartSet fSet1, toCartSet fSet2) 
    | ListSet(fList)                        -> ElemList(toListList fList)
    | ElemList(_)                           -> failwith "Error: Trying to mutate already mutated AST (List Set)"
    | INTSet                                -> ast
    | STRSet                                -> ast
    
// Flatten a domain by merging same level cartesian domains    
let rec reduceDom (ast : domain) : domain = 
    match ast with
    | PowersetDom(fSet)                     -> PowersetDom(toCartSet fSet)
    | TotalFunctionSpaceDom(fSset, fDom)    -> TotalFunctionSpaceDom(fSset, reduceDom fDom) //SSet are already reduced by default
    | CartesianDom(fDom1, fDom2)            ->  match fDom1 with
                                                | CartesianDom(_)       -> CartesianListDom( getValueCLD (reduceDom fDom1)  @ [reduceDom fDom2] )
                                                | _                     -> CartesianListDom([reduceDom fDom1] @ [reduceDom fDom2])                                            
    | CartesianListDom(_)                   -> failwith "Error: Trying to mutate already mutated AST (Cartesian Dom)"
    | SetDom(fset)                          -> SetDom(toCartSet fset)
