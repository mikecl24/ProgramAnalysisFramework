open System
#load "DomainTypes.fs"
open DomainTypes                    // Types: AST

type DomainGenType = {
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

let evalCartListItem (fListSet, pNum, mNum, rNum, csNum, vNum, qNum, uNum, lNum, resultString, prependString) =
    match fListSet with
    | QSet                      ->  {result = "Q" + qNum.ToString() + " : Node ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum;
                                    q = qNum+1;
                                    u = uNum;
                                    l = lNum;
                                    prepend = prependString}
    | VARSet                    ->  {result = "VAR" + vNum.ToString() + " : Var ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum+1;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = prependString}
    | UnionSet(fSet1, fSet2)    ->  {result = "Unimplemented UnionSet in evalCartListItem\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = prependString}
    | ListSet(fList)            ->  {result = "Unimplemented ListSet in evalCartListItem\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    cs = csNum;
                                    v = vNum;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = prependString}
    | _                         -> failwith "Error matching set in evalCartListItem"



let rec evalCartListSet (fListSet, pNum, mNum, rNum, csNum, vNum, qNum, uNum, lNum, resultString, prependString) =
    match fListSet with
    | []    ->  {result = resultString;
                p = pNum;
                m = mNum;
                r = rNum;
                cs = csNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                prepend = prependString}
    | a::b  ->  let prev = evalCartListItem (a, pNum, mNum, rNum, csNum, vNum, qNum, uNum, lNum, resultString, prependString)
                evalCartListSet (b, prev.p, prev.m, prev.r, prev.cs, prev.v, prev.q, prev.u, prev.l, resultString+prev.result, prev.prepend+prependString)

let evalSet (ast, pNum, mNum, rNum, csNum, vNum, qNum, uNum, lNum, resultString) = 
    match ast with
    | CartesianListSet(fListSet)    ->  let prev = evalCartListSet (fListSet, pNum, mNum, rNum+1, csNum, vNum, qNum, uNum, lNum, resultString, "")
                                        {result = prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        cs = csNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | QSet                          ->  {result = "\ntype Q" + qNum.ToString() + " = Q" + qNum.ToString() + " of Node\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        cs = csNum;
                                        v = vNum;
                                        q = qNum+1;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | VARSet                        ->  {result = "\ntype VAR" + vNum.ToString() + " = VAR" + vNum.ToString() + " of Var\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        cs = csNum;
                                        v = vNum+1;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | UnionSet(fSet1, fSet2)        ->  {result = "\nUnimplemented UnionSet in evalSet\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        cs = csNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | ListSet(fList)                ->  {result = "\nUnimplemented ListSet in evalSet\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        cs = csNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | _                             -> failwith "Error detecting set at evalSet"



let powersetString pNum next= ("\ntype Powerset" + (pNum.ToString()) +  " = " + next + " Set\n")

let evalPowerset (typeNext, pNum, csNum, vNum, qNum, uNum, lNum) =
    match typeNext with
    | CartesianListSet(fListSet)    -> (powersetString pNum ("Cartesian" +  csNum.ToString()))
    | QSet                          -> (powersetString pNum ("Q" +  qNum.ToString()))
    | VARSet                        -> (powersetString pNum ("VAR" + vNum.ToString()))
    | UnionSet(fSet1, fSet2)        -> (powersetString pNum ("Union" +  uNum.ToString()))
    | ListSet(fList)                -> (powersetString pNum ("List" +  lNum.ToString()))
    | _                             -> failwith "Error detecting set type at evalPowerset"

let rec evalDom (ast, pNum, mNum, rNum, csNum, vNum, qNum, uNum, lNum, resultString) = 
    match ast with
    | PowersetDom(fSet)                     ->  let prev = evalSet (fSet, pNum+1, mNum, rNum, csNum, vNum, qNum, uNum, lNum, resultString);
                                                {result = prev.result + (evalPowerset (fSet, pNum, csNum, vNum, qNum, uNum, lNum)) + resultString;
                                                p = prev.p;
                                                m = prev.m;
                                                r = prev.r;
                                                cs = prev.cs;
                                                v = prev.v;
                                                q = prev.q;
                                                u = prev.u;
                                                l = prev.l;
                                                prepend = prev.prepend}
    | TotalFunctionSpaceDom(fSet, fDom)     -> {result = "\nUnimplemented TotalFunctionSpaceDom in evalDom\n";
                                                p = pNum;
                                                m = mNum;
                                                r = rNum;
                                                cs = csNum;
                                                v = vNum;
                                                q = qNum;
                                                u = uNum;
                                                l = lNum;
                                                prepend = ""}
    | CartesianListDom(fListDom)            -> {result = "\nUnimplemented CartesianListDom in evalDom\n";
                                                p = pNum;
                                                m = mNum;
                                                r = rNum;
                                                cs = csNum;
                                                v = vNum;
                                                q = qNum;
                                                u = uNum;
                                                l = lNum;
                                                prepend = ""}
    | _                                     -> failwith "Error matching ast in evalDom"


let header = "module Domain\n\n// Generated Code Section: Domain type\n(*\nQ -> "

let analysisType ast = 
    match ast with
    | PowersetDom(fSet)                     -> "Powerset1"
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map1"
    | CartesianListDom(fListDom)            -> "Record1"
    | _                                     -> failwith "Error matching start of domain"

let format (genTypes, textForm, ast) = (header + textForm + "\n*)\n\ntype Node = Node of int\ntype Var = Var of string\n\n" 
                                        + genTypes + "\ntype AnalysisResult = AnalysisResult of Map<Node," + (analysisType ast) + ">")

let evaluateAST ast textForm = format ( ((evalDom (ast, 1, 1, 1, 1, 1, 1, 1, 1, "")).result ), textForm, ast )

let ast1 = PowersetDom(CartesianListSet[VARSet; UnionSet(QSet, ListSet(Element "QM")); QSet])
let ast2 = TotalFunctionSpaceDom(QSet,PowersetDom(CartesianListSet [VARSet; UnionSet (QSet, ListSet (Element "QM")); QSet]))
printfn "%s" (evaluateAST ast1 "P( VAR * [Q U {QM}] * Q )")
printfn "\n%A" ast1