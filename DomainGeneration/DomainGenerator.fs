module DomainGenerator

type recordSetType = {
    result : string;
    p : int;
    m : int;
    r : int;
    cs : int;
    v : int;
    q : int;
    u : int;
    l : int;
    prepend : string
}

let rec evalList ast = 
    match ast with
    | Element(fElem)                        -> "test"
    | LargerList(fElem, fList)              -> "test"

let genCartSet (set, rNum, csNum, vNum, qNum, uNum, lNum) = 
    match set with
    | QSet                      -> {result = ("Q" + qNum.ToString() + " : int;\n");
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum;
                                    q = qNum+1;
                                    u = uNum;
                                    l = lNum;
                                    prepend = ""
                                    }
    | VARSet                    -> {result = ("VAR" + vNum.ToString() + " : string;\n");
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum+1;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = ""
                                    }
    | UnionSet(fSet1, fSet2)    -> {result = ("UNION" + uNum.ToString() + " : Union" + uNum.ToString() + ";\n");
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = ""
                                    }
    | ListSet(fList)            -> {result = "?";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = ""
                                    }
    | _                         -> failwith "Error invalid match in genCartSet"

let rec evalCartSet (setlist, rNum, csNum, vNum, qNum, uNum, lNum, resultString, prep) = 
    match setlist with
    | []    -> resultString
    | a::b  -> resultString
    (*
    if (getLast (set1.GetType().ToString().Split '+')) = "CartesianListSet" 
        then "nice\n\n" + resultString
    else 
        let intermediary = (evalCartSet (set1, rNum+1, csNum, vNum, qNum, uNum, lNum) )
        let final = (evalCartSet (set2, intermediary.r, intermediary.cs, intermediary.v, intermediary.q, intermediary.u, intermediary.l) )
        intermediary.prepend + final.prepend + "let Record" + rNum.ToString() + " = \n{ " + intermediary.result + final.result + "}\n\n" + resultString
        *)

let rec evalSet ast rNum csNum vNum qNum uNum lNum resultString = 
    match ast with
    | CartesianListSet(fListSet)        -> (evalCartSet (fListSet, rNum, csNum, vNum, qNum, uNum, lNum, resultString, ""))
    | QSet                              -> {result = "?";
                                            p = pNum;
                                            m = mNum;
                                            r = rNum;
                                            cs = csNum;
                                            v = vNum;
                                            q = qNum;
                                            u = uNum;
                                            l = lNum;
                                            prepend = ""
                                            }
    | VARSet                            -> {result = "?";
                                            p = pNum;
                                            m = mNum;
                                            r = rNum;
                                            cs = csNum;
                                            v = vNum;
                                            q = qNum;
                                            u = uNum;
                                            l = lNum;
                                            prepend = ""
                                            }
    | UnionSet(fSet1, fSet2)            -> {result = "?";
                                            p = pNum;
                                            m = mNum;
                                            r = rNum;
                                            cs = csNum;
                                            v = vNum;
                                            q = qNum;
                                            u = uNum;
                                            l = lNum;
                                            prepend = ""
                                            }
    | ListSet(fList)                    -> {result = "?";
                                            p = pNum;
                                            m = mNum;
                                            r = rNum;
                                            cs = csNum;
                                            v = vNum;
                                            q = qNum;
                                            u = uNum;
                                            l = lNum;
                                            prepend = ""
                                            }
    | _                                 -> failwith "Error matching evalSet"

let totalfunctionspaceString mNum = "type Map" + mNum.ToString() + " = Map" + mNum.ToString() + " of Map<int, int>"

let powersetString setNr next= ("type Powerset" + (setNr.ToString()) +  " = " + next + " Set\n\n")

let evalPowerset (typeNext, pNum, csNum, vNum, qNum, uNum, lNum) =
    match typeNext with
    | CartesianListSet(fListSet)    -> (powersetString pNum ("Cartesian" +  csNum.ToString()))
    | QSet                          -> "todo"
    | VARSet                        -> "todo"
    | UnionSet(fSet1, fSet2)        -> "todo"
    | ListSet(fList)                -> "todo"
    | _                             -> failwith "Failure detecting Powerset at type"

let rec evalDom ast pNum mNum rNum csNum vNum qNum uNum lNum resultString = 
    match ast with
    | PowersetDom(fSet)                     -> {result = (evalSet fSet rNum csNum vNum qNum uNum lNum 
                                                    (evalPowerset (fSet, pNum, csNum, vNum, qNum, uNum, lNum) + resultString));
                                                p = pNum+1;
                                                m = mNum;
                                                r = rNum;
                                                cs = csNum;
                                                v = vNum;
                                                q = qNum;
                                                u = uNum;
                                                l = lNum;
                                                prepend = ""
                                                }
    | TotalFunctionSpaceDom(fSet, fDom)     -> {result = (totalfunctionspaceString mNum);
                                                p = pNum;
                                                m = mNum;
                                                r = rNum;
                                                cs = csNum;
                                                v = vNum;
                                                q = qNum;
                                                u = uNum;
                                                l = lNum;
                                                prepend = ""
                                                }
    | CartesianListDom(fListDom)            -> {result = "Unimplemented CartesianListDom in evalDom";
                                                p = pNum;
                                                m = mNum;
                                                r = rNum;
                                                cs = csNum;
                                                v = vNum;
                                                q = qNum;
                                                u = uNum;
                                                l = lNum;
                                                prepend = ""
                                                }
    | _                                     -> failwith "Error matching domain"
let header = "module Domain

// Generated Code Section: Domain type
(*\nQ -> "

let analysisType ast = 
    match ast with
    | PowersetDom(fSet)                     -> "Powerset1"
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map1"
    | CartesianListDom(fListDom)            -> "Record1"
    | _                                     -> failwith "Error matching start of domain"

let format genTypes textForm ast = (header + textForm + "\n*)\n\n" + genTypes + "type AnalysisResult = AnalysisResult of Map<int," + (analysisType ast) + ">")

let evaluateAST ast textForm = (format (evalDom ast 1 1 1 1 1 1 1 1 "").result textForm ast)