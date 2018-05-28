[<AutoOpen>]
module CallGenerator

// let fst3 (a, b, c) = a
// let snd3 (a, b, c) = b
// let trd3 (a, b, c) = c

let getDescriptor (ast : sset) : string = 
    match ast with
    | QSet      -> "Nodes"
    | VARSet    -> "Variables"
    | ARRSet    -> "Arrays"
    | IDENTSet  -> "Identifiers"

let getTypeName (ast : sset) : string = 
    match ast with
    | QSet      -> "Node"
    | VARSet    -> "Var"
    | ARRSet    -> "Arr"
    | IDENTSet  -> "Ident"

let rec checkCartSet (setl : set list, mNum : int, oldM : Map<int, (string*string)>) : (bool * int * Map<int, (string*string)>) = 
    match setl with
    | []    -> (true, mNum, oldM)
    | a::b  ->  let prev = checkSet (a, mNum, oldM)
                let next = checkCartSet (b, snd3 prev, trd3 prev)
                (fst3 prev && fst3 next, snd3 next, trd3 next)

and checkSet (ast : set, mNum : int, oldM : Map<int, (string*string)>) : (bool * int * Map<int, (string*string)>) = 
    match ast with
    | SsetSet(ss)                   -> (true, mNum, oldM)
    | CartesianSet(_)               -> failwith "Error: Attempting to process non-consolidated domain in Set (Cartesian Set)"
    | CartesianListSet(ls)          -> checkCartSet (ls, mNum, oldM) //MARK
    | TotalFunctionSpaceSet(ss, s)  -> checkSet (s, mNum+1, oldM.Add(mNum, (getDescriptor ss, getTypeName ss)))
    | PowersetSet(s)                -> let prev = checkSet (s, mNum, oldM);
                                        in if (not (fst3 prev)) then 
                                            failwith "Error: Invalid domain (using powerset set with an infinite set inside)"
                                           else
                                            prev
    | UnionSet(s1, s2)              -> (true, mNum, oldM) //MARK
    | ListSet(_)                    -> failwith "Error: Attempting to process non-consolidated domain in Set (List Set)"
    | ElemList(sl)                  -> (true, mNum, oldM)
    | INTSet                        -> (false, mNum, oldM)
    | STRSet                        -> (false, mNum, oldM)

let rec checkCartDom (doml : domain list, mNum : int, oldM : Map<int, (string*string)>) : (bool * int * Map<int, (string*string)>) = 
    match doml with
    | []    -> (true, mNum, oldM)
    | a::b  ->  let prev = checkDom (a, mNum, oldM)
                let next = checkCartDom (b, snd3 prev, trd3 prev)
                (fst3 prev && fst3 next, snd3 next, trd3 next)


and checkDom (dom : domain, mNum : int, oldM : Map<int, (string*string)>) : (bool * int * Map<int, (string*string)>) = 
    match dom with
    | PowersetDom(s)                ->  let prev = checkSet (s, mNum, oldM);
                                        in if (not (fst3 prev)) then 
                                            failwith "Error: Invalid domain (using powerset domain with an infinite set inside)"
                                           else
                                            prev
    | TotalFunctionSpaceDom(ss, d)  ->  checkDom (d, mNum+1, oldM.Add(mNum, (getDescriptor ss, getTypeName ss)))
    | CartesianDom(_)               ->  failwith "Error: Attempting to process non-consolidated domain in Domain (Cartesian Dom)"
    | CartesianListDom(dl)          ->  let prev = checkCartDom (dl, mNum, oldM)
                                        in if (not (fst3 prev)) then
                                            failwith "Error: Invalid domain (Cartesian domain product with unsafe domains)"
                                           else
                                            prev
    | SetDom(s)                     ->  let prev = checkSet (s, mNum, oldM)
                                        in (false, snd3 prev, trd3 prev)

let domCheck (dom : domain) : (bool * int * Map<int, (string*string)>) = 
    checkDom (dom, 1, Map.empty)