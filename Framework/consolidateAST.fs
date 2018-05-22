[<AutoOpen>]
module consolidateAST
open System.Runtime.InteropServices

let getValueCLS (v) = 
    match v with
    | CartesianListSet v    ->  v
    | _                     ->  failwith "Extraction attempt with wrong funciton: CLS"

let getValueCLD (v) = 
    match v with
    | CartesianListDom v    ->  v
    | _                     -> failwith "Extraction attempt with wrong funciton: CLD"
//let getValueEL (ElemList v) = v

let rec getLast (array : string []) = 
    array.[array.Length - 1]

let rec toListList ast =  
    match ast with
    | Element(fElem)                        -> [ fElem ]
    | LargerList(fElem, fList)              -> match fList with
                                                | LargerList(fElem2, fList2)    -> [fElem] @ (toListList fList)
                                                | Element(fElem3)               -> [ fElem ] @ [ fElem3 ] 


let rec toCartSet ast =  
    match ast with
    | CartesianSet(fSet1, fSet2)            -> match fSet1 with
                                                | CartesianSet(f1, f2)  -> CartesianListSet( getValueCLS (toCartSet fSet1)  @ [toCartSet fSet2] )
                                                | _                     -> CartesianListSet([toCartSet fSet1] @ [toCartSet fSet2]) 
    | QSet                                  -> QSet
    | VARSet                                -> VARSet
    | ARRSet                                -> ARRSet
    | UnionSet(fSet1, fSet2)                -> UnionSet(toCartSet fSet1, toCartSet fSet2) 
    | ListSet(fList)                        -> ElemList(toListList fList)
    | PowersetSet(fSet)                     -> PowersetSet( toCartSet fSet )
    | TotalFunctionSpaceSet(fset1, fset2)   -> TotalFunctionSpaceSet( toCartSet fset1 , toCartSet fset2)
    | _                                     -> failwith "Already mutated AST"

let rec reduceDom ast = 
    match ast with
    | PowersetDom(fSet)                     -> PowersetDom(toCartSet fSet)
    | TotalFunctionSpaceDom(fSet, fDom)     -> TotalFunctionSpaceDom(toCartSet fSet, reduceDom fDom)
    | CartesianDom(fDom1, fDom2)            ->  match fDom1 with
                                                | CartesianDom(f1, f2)  -> CartesianListDom( getValueCLD (reduceDom fDom1)  @ [reduceDom fDom2] )
                                                | _                     -> CartesianListDom([reduceDom fDom1] @ [reduceDom fDom2]) 
    | _                                     -> failwith "Already mutated AST"

let generateCode (ast, dstring) = 
    match ast with
    | DomainMetaL(d)  ->  // Domain AST -> flattened Domain AST
                        let flatDomainAST : domainM = reduceDom (d)
                        //printfn "%s" (flatDomainAST.ToString())

                        // flattened Domain AST -> Types
                        let code = evaluateAST flatDomainAST dstring
                        //printfn "%s" code

                        // flattened Domain AST -> Lattice Operations
                        let lattOps = evaluateASTCalls flatDomainAST
                        //printfn "%s" lattOps


                        File.WriteAllText("Domain.fs", code + lattOps)

    | DomainGraph(d)  ->  failwith "nice"