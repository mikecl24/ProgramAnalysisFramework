module DomainGenerator

let rec evalList ast = 
    match ast with
    | Element(fElem)                        -> "test"
    | LargerList(fElem, fList)              -> "test"

let rec evalSet ast pNum csNum resultString = 
    match ast with
    | CartesianSet(fSet1, fSet2)            -> ("test1\n\n" + resultString)
    | QSet                                  -> "test"
    | VARSet                                -> "test"
    | UnionSet(fSet1, fSet2)                -> "test"
    | ListSet(fList)                        -> "test"



let powersetString setNr next= ("type Powerset" + (setNr.ToString()) +  " = " + next + " Set\n\n")

let evalPowerset (typeNext, pNum, csNum, qNum) =
    match typeNext with
    | "CartesianSet"    -> (powersetString pNum ("Cartesian" +  csNum.ToString()))
    | "QSet"            -> "todo"
    | "VARSet"          ->
    | "UnionSet"        ->
    | "ListSet"         ->
    | _ -> failwith ("Failure detecting Powerset at type " + typeNext)

let rec evalDom ast pNum csNum qNum resultString = 
    match ast with
    | PowersetDom(fSet)                     -> (evalSet fSet (pNum+1) csNum (resultString + evalPowerset (((fSet.GetType().ToString().Split '+').[2]), pNum, csNum, qNum)))
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Issues with maps not being nice"
    | CartesianDom(fDom1, fDom2)            -> "Problem with keeping numbers fresh -> do one brach 
                                                -> return new nums -> call other with this (make all dom outputs this format)"

let header = "module Domain

// Generated Code Section: Domain type
(*\nQ -> "

let format genTypes textForm = (header + textForm + "\n*)\n\n" + genTypes + "\n\nlet AnalysisResult = Map.empty")

let evaluateAST ast textForm = (format (evalDom ast 1 1 1 "") textForm)
