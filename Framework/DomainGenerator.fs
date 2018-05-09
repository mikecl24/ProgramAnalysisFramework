[<AutoOpen>]
module DomainGenerator

type DomainGenType = {
    result : string;
    p : int;
    m : int;
    r : int;
    v : int;
    q : int;
    u : int;
    l : int;
    prepend : string
}

let lookAheadDom (ast, p, m, r) = 
    match ast with
    | PowersetDom(fSet)                     -> "Powerset"+p.ToString()
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map"+m.ToString()
    | CartesianListDom(fListDom)            -> "ComplexDomain"
    | _                                     -> failwith "Error matching start of domain"

let lookAheadSet (ast, r, v, q, u, l) = 
    match ast with
    | CartesianListSet(fListSet)            -> "Record"+r.ToString()
    | QSet                                  -> "Node"
    | VARSet                                -> "Var"
    | UnionSet(fSet1, fSet2)                -> "Union"+u.ToString()
    | ElemList(fList)                       -> "List"+l.ToString()
    | _                                     -> failwith "Error matching start of domain"

let lookAheadDomRecord (ast, p, m, r) =
    match ast with
    | PowersetDom(fSet)                     -> "Powerset"+p.ToString() + " * "
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map"+m.ToString() + " * "
    | _                                     -> failwith "Error matching start of domain"

let rec evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString, prependString) =
    match fList with
    | []    ->  {result = "\ntype List" + lNum.ToString() + " =\n" + resultString + "\n";
                p = pNum;
                m = mNum;
                r = rNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                prepend = ""}
    | a::b  ->  evalList (b, pNum, mNum, rNum, vNum, qNum, uNum, lNum, ( resultString + "    | " + a + "\n" ), prependString)

let rec evalUnionSet (fSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString, prependString) =
    match fSet with
    | CartesianListSet(fListSet)    ->  let prev = evalCartListSet (fListSet, pNum, mNum, rNum+1, vNum, qNum, uNum, lNum, resultString, "")
                                        {result = "    | Record" + rNum.ToString() + " of Record" + rNum.ToString() + "\n";
                                        p = prev.p;
                                        m = prev.m;
                                        r = prev.r;
                                        v = prev.v;
                                        q = prev.q;
                                        u = prev.u;
                                        l = prev.l;
                                        prepend =  prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n";}
    | QSet                          ->  {result = "    | Q" + qNum.ToString() + " of Node \n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum+1;
                                        u = uNum;
                                        l = lNum;
                                        prepend = prependString}
    | VARSet                        ->  {result = "    | Var" + vNum.ToString() + " of Node \n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum+1;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = prependString}
    | UnionSet(fSet1, fSet2)        ->  let first = evalUnionSet (fSet1, pNum, mNum, rNum, vNum, qNum, uNum+1, lNum, "", "")
                                        let second = evalUnionSet (fSet2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, "", "")
                                        {result = "    | Union" + uNum.ToString() + " of Union" + uNum.ToString();
                                        p = second.p;
                                        m = second.m;
                                        r = second.r;
                                        v = second.v;
                                        q = second.q;
                                        u = second.u;
                                        l = second.l;
                                        prepend = first.prepend + second.prepend + 
                                            "\ntype Union" + uNum.ToString() + " =\n" + first.result + second.result + prependString+"\n"}
    | ElemList(fList)                -> let listRes = evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, "", "")
                                        {result = "    | List" + lNum.ToString() + " of List" + lNum.ToString() + "\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum+1;
                                        prepend = listRes.result + prependString}
    | _                             ->  failwith "Error matching evalUnionSet"

and evalCartListItem (fListSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString, prependString) =
    match fListSet with
    | QSet                      ->  {result = "    Q" + qNum.ToString() + " : Node ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    v = vNum;
                                    q = qNum+1;
                                    u = uNum;
                                    l = lNum;
                                    prepend = prependString}
    | VARSet                    ->  {result = "    Var" + vNum.ToString() + " : Var ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    v = vNum+1;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    prepend = prependString}
    | UnionSet(fSet1, fSet2)    ->  let first = evalUnionSet (fSet1, pNum, mNum, rNum, vNum, qNum, uNum+1, lNum, "", "")
                                    let second = evalUnionSet (fSet2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, "", "")
                                    {result = "    Union" + uNum.ToString() + " : Union" + uNum.ToString() + ";\n";
                                    p = second.p;
                                    m = second.m;
                                    r = second.r;
                                    v = second.v;
                                    q = second.q;
                                    u = second.u;
                                    l = second.l;
                                    prepend = first.prepend + second.prepend + 
                                            "\ntype Union" + uNum.ToString() + " =\n" + first.result + second.result + prependString+"\n"}
    | ElemList(fList)            -> let first = evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, "", "")
                                    {result = "    List"  + lNum.ToString() + " : List" + lNum.ToString() + ";\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    v = vNum;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum+1;
                                    prepend = first.result + prependString}
    | _                         -> failwith "Error matching set in evalCartListItem"

and evalCartListSet (fListSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString, prependString) =
    match fListSet with
    | []    ->  {result = resultString;
                p = pNum;
                m = mNum;
                r = rNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                prepend = prependString}
    | a::b  ->  let prev = evalCartListItem (a, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString, prependString)
                evalCartListSet (b, prev.p, prev.m, prev.r, prev.v, prev.q, prev.u, prev.l, resultString+prev.result, prev.prepend)

let rec evalSet (ast, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString) = 
    match ast with
    | CartesianListSet(fListSet)    ->  let prev = evalCartListSet (fListSet, pNum, mNum, rNum+1, vNum, qNum, uNum, lNum, resultString, "")
                                        {result = prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n";
                                        p = prev.p;
                                        m = prev.m;
                                        r = prev.r;
                                        v = prev.v;
                                        q = prev.q;
                                        u = prev.u;
                                        l = prev.l;
                                        prepend = ""}
    | QSet                          ->  {result = "";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | VARSet                        ->  {result = "";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | UnionSet(fSet1, fSet2)        ->  let first = evalUnionSet (fSet1, pNum, mNum, rNum, vNum, qNum, uNum+1, lNum, "", "")
                                        let second = evalUnionSet (fSet2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, "", "" )
                                        {result = first.prepend + second.prepend + "\ntype Union" + uNum.ToString() + " = \n" + first.result + second.result + "\n";
                                        p = second.p;
                                        m = second.m;
                                        r = second.r;
                                        v = second.v;
                                        q = second.q;
                                        u = second.u;
                                        l = second.l;
                                        prepend = ""}
    | ElemList(fList)                -> let first = evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, "", "")
                                        {result = first.result;
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        prepend = ""}
    | _                             -> failwith "Error detecting set at evalSet"



let powersetString pNum next= ("\ntype Powerset" + (pNum.ToString()) +  " = " + next + " Set\n")

let evalPowerset (typeNext, pNum, rNum, vNum, qNum, uNum, lNum) =
    match typeNext with
    | CartesianListSet(fListSet)    -> (powersetString pNum ("Record" +  rNum.ToString()))
    | QSet                          -> (powersetString pNum ("Node"))
    | VARSet                        -> (powersetString pNum ("Var"))
    | UnionSet(fSet1, fSet2)        -> (powersetString pNum ("Union" +  uNum.ToString()))
    | ElemList(fList)                -> (powersetString pNum ("List" +  lNum.ToString()))
    | _                             -> failwith "Error detecting set type at evalPowerset"

let rec evalDomList (fListDom, pNum, mNum, rNum, vNum, qNum, uNum, lNum, (resultString:string), prependString) = 
    match fListDom with
    | []    -> {result = (resultString.Remove(resultString.Length-2));  // remove extra asterisk
                p = pNum;
                m = mNum;
                r = rNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                prepend = prependString}
    | a::b  ->  let prev = evalDom (a, pNum, mNum, rNum, vNum, qNum, uNum, lNum, "")
                evalDomList (b, prev.p, prev.m,  prev.r, prev.v, prev.q, prev.u, prev.l, resultString + lookAheadDomRecord (a, pNum, mNum, rNum), prev.result + prependString)

and evalDom (ast, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString) = 
    match ast with
    | PowersetDom(fSet)                     ->  let prev = evalSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, resultString);
                                                {result = prev.result + (evalPowerset (fSet, pNum, rNum, vNum, qNum, uNum, lNum)) + resultString;
                                                p = prev.p;
                                                m = prev.m;
                                                r = prev.r;
                                                v = prev.v;
                                                q = prev.q;
                                                u = prev.u;
                                                l = prev.l;
                                                prepend = prev.prepend}
    | TotalFunctionSpaceDom(fSet, fDom)     ->  let first = evalSet(fSet, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, "")
                                                let second = evalDom(fDom, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.result)
                                                {result = second.result + "\ntype Map" + mNum.ToString() + " = " + "Map<" + (lookAheadSet (fSet, rNum, vNum, qNum, uNum, lNum)) + "," + (lookAheadDom (fDom, pNum, mNum, rNum)) + ">\n"
                                                    + resultString;
                                                p = second.p;
                                                m = second.m;
                                                r = second.r;
                                                v = second.v;
                                                q = second.q;
                                                u = second.u;
                                                l = second.l;
                                                prepend = ""}
    | CartesianListDom(fListDom)            ->  let resDom = evalDomList (fListDom, pNum, mNum, rNum, vNum, qNum, uNum, lNum, resultString, "")
                                                {result = resDom.prepend + "\ntype ComplexDomain"+" = " + resDom.result + "\n";
                                                p = resDom.p;
                                                m = resDom.m;
                                                r = resDom.r;
                                                v = resDom.v;
                                                q = resDom.q;
                                                u = resDom.u;
                                                l = resDom.l;
                                                prepend = ""}
    | _                                     -> failwith "Error matching ast in evalDom"


let header = "[<AutoOpen>]\nmodule Domain\n\n// Generated Code Section: Domain type\n(*\nQ -> "



let format (genTypes, textForm, ast) = (header + textForm + "\n*)\n\n" 
                                        + genTypes + "\ntype sigma = " + (lookAheadDom (ast, 1, 1, 1)) + "\n\ntype AnalysisResult = Map<Node, sigma>\n")

let evaluateAST ast textForm = format ( ((evalDom (ast, 1, 1, 1, 1, 1, 1, 1, "")).result ), textForm, ast )