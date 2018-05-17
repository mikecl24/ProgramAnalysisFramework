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
    a : int
    prepend : string
}

let powersetString pNum next= ("\ntype Powerset" + (pNum.ToString()) +  " = " + next + " Set\n")
let evalPowerset (typeNext, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum) =
    match typeNext with
    | CartesianListSet(fListSet)    -> (powersetString pNum ("Record" +  rNum.ToString()))
    | QSet                          -> (powersetString pNum ("Node"))
    | VARSet                        -> (powersetString pNum ("Var"))
    | ARRSet                        -> (powersetString pNum ("Arr"))
    | UnionSet(fSet1, fSet2)        -> (powersetString pNum ("Union" +  uNum.ToString()))
    | ElemList(fList)               -> (powersetString pNum ("List" +  lNum.ToString()))
    | PowersetSet(fSet)             -> (powersetString pNum ("Powerset" +  (pNum+1).ToString()))
    | TotalFunctionSpaceSet(s1, s2) -> (powersetString pNum ("Map" +  (mNum).ToString()))
    | _                             -> failwith "Error detecting set type at evalPowerset"


let lookAheadDom (ast, p, m, r) = 
    match ast with
    | PowersetDom(fSet)                     -> "Powerset"+p.ToString()
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map"+m.ToString()
    | CartesianListDom(fListDom)            -> "ComplexDomain"
    | _                                     -> failwith "Error matching start of domain"

let lookAheadSet (ast, p, m, r, v, q, u, l, a) = 
    match ast with
    | CartesianListSet(fListSet)            -> "Record"+r.ToString()
    | QSet                                  -> "Node"
    | VARSet                                -> "Var"
    | ARRSet                                -> "Arr"
    | UnionSet(fSet1, fSet2)                -> "Union"+u.ToString()
    | ElemList(fList)                       -> "List"+l.ToString()
    | PowersetSet(fSet)                     -> "Powerset"+p.ToString()
    | TotalFunctionSpaceSet(s1, s2)         -> "Map"+m.ToString()
    | _                                     -> failwith "Error matching start of domain"

let lookAheadDomRecord (ast, p, m) =
    match ast with
    | PowersetDom(fSet)                     -> "Powerset" + p.ToString() + " * "
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map" + m.ToString() + " * "
    | _                                     -> failwith "Error matching start of domain"

let rec evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString, prependString) =
    match fList with
    | []    ->  {result = "\ntype List" + lNum.ToString() + " =\n" + resultString + "\n";
                p = pNum;
                m = mNum;
                r = rNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                a = aNum;
                prepend = ""}
    | a::b  ->  evalList (b, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, ( resultString + "    | " + a + "\n" ), prependString)

let rec evalUnionSet (fSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString, prependString) =
    match fSet with
    | CartesianListSet(fListSet)    ->  let prev = evalCartListSet (fListSet, pNum, mNum, rNum+1, vNum, qNum, uNum, lNum, aNum,  resultString, "")
                                        {result = "    | Record" + rNum.ToString() + " of Record" + rNum.ToString() + "\n";
                                        p = prev.p;
                                        m = prev.m;
                                        r = prev.r;
                                        v = prev.v;
                                        q = prev.q;
                                        u = prev.u;
                                        l = prev.l;
                                        a = prev.a;
                                        prepend =  prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n";}
    | QSet                          ->  {result = "    | Q" + qNum.ToString() + " of Node \n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum+1;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum;
                                        prepend = prependString}
    | VARSet                        ->  {result = "    | Var" + vNum.ToString() + " of Var \n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum+1;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum;
                                        prepend = prependString}
    | ARRSet                        ->  {result = "    | Arr" + aNum.ToString() + " of Arr \n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum+1;
                                        prepend = prependString}
    | UnionSet(fSet1, fSet2)        ->  let first = evalUnionSet (fSet1, pNum, mNum, rNum, vNum, qNum, uNum+1, lNum, aNum, "", "")
                                        let second = evalUnionSet (fSet2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, "", "")
                                        {result = "    | Union" + uNum.ToString() + " of Union" + uNum.ToString();
                                        p = second.p;
                                        m = second.m;
                                        r = second.r;
                                        v = second.v;
                                        q = second.q;
                                        u = second.u;
                                        l = second.l;
                                        a = second.a;
                                        prepend = first.prepend + second.prepend + 
                                            "\ntype Union" + uNum.ToString() + " =\n" + first.result + second.result + prependString+"\n"}
    | ElemList(fList)                -> let listRes = evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, "", "")
                                        {result = "    | List" + lNum.ToString() + " of List" + lNum.ToString() + "\n";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum+1;
                                        a = aNum;
                                        prepend = listRes.result + prependString}
    | PowersetSet(fSet)             ->  let prev = evalSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, aNum,  resultString)
                                        {result = "    | Powerset" + pNum.ToString() + " of " + lookAheadSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, aNum) + " Set \n";
                                        p = prev.p;
                                        m = prev.m;
                                        r = prev.r;
                                        v = prev.v;
                                        q = prev.q;
                                        u = prev.u;
                                        l = prev.l;
                                        a = prev.a;
                                        prepend =  prev.prepend + prev.result + "\n";}
    | TotalFunctionSpaceSet(s1, s2) ->  let first = evalSet (s1, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, aNum,  resultString)
                                        let second = evalSet (s2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, resultString)
                                        {result = "    | Map" + mNum.ToString() + " of Map<" + lookAheadSet (s1, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, aNum) + ", " + lookAheadSet (s2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a) + "> \n";
                                        p = second.p;
                                        m = second.m;
                                        r = second.r;
                                        v = second.v;
                                        q = second.q;
                                        u = second.u;
                                        l = second.l;
                                        a = second.a;
                                        prepend =  first.prepend + first.result + second.prepend + second.result + "\n";}
    | _                             ->  failwith "Error matching evalUnionSet"

and evalCartListItem (fListSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString, prependString) =
    match fListSet with
    | QSet                      ->  {result = "    Q" + qNum.ToString() + " : Node ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    v = vNum;
                                    q = qNum+1;
                                    u = uNum;
                                    l = lNum;
                                    a = aNum;
                                    prepend = prependString}
    | VARSet                    ->  {result = "    Var" + vNum.ToString() + " : Var ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    v = vNum+1;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    a = aNum;
                                    prepend = prependString}
    | ARRSet                    ->  {result = "    Arr" + aNum.ToString() + " : Arr ;\n";
                                    p = pNum;
                                    m = mNum;
                                    r = rNum;
                                    v = vNum;
                                    q = qNum;
                                    u = uNum;
                                    l = lNum;
                                    a = aNum+1;
                                    prepend = prependString}
    | UnionSet(fSet1, fSet2)    ->  let first = evalUnionSet (fSet1, pNum, mNum, rNum, vNum, qNum, uNum+1, lNum, aNum, "", "")
                                    let second = evalUnionSet (fSet2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, "", "")
                                    {result = "    Union" + uNum.ToString() + " : Union" + uNum.ToString() + ";\n";
                                    p = second.p;
                                    m = second.m;
                                    r = second.r;
                                    v = second.v;
                                    q = second.q;
                                    u = second.u;
                                    l = second.l;
                                    a = second.a;
                                    prepend = first.prepend + second.prepend + 
                                            "\ntype Union" + uNum.ToString() + " =\n" + first.result + second.result + prependString+"\n"}
    | ElemList(fList)           -> let first = evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, "", "")
                                   {result = "    List"  + lNum.ToString() + " : List" + lNum.ToString() + ";\n";
                                   p = pNum;
                                   m = mNum;
                                   r = rNum;
                                   v = vNum;
                                   q = qNum;
                                   u = uNum;
                                   l = lNum+1;
                                   a = aNum;
                                   prepend = first.result + prependString}
    | PowersetSet(fSet)            -> let prev = evalSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString)
                                      {result = "    Powerset"  + pNum.ToString() + " : " + lookAheadSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, aNum) + " Set;\n";
                                       p = prev.p;
                                       m = prev.m;
                                       r = prev.r;
                                       v = prev.v;
                                       q = prev.q;
                                       u = prev.u;
                                       l = prev.l;
                                       a = prev.a;
                                       prepend = prev.prepend + prev.result}
    | TotalFunctionSpaceSet(s1, s2) ->  let first = evalSet (s1, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, aNum, resultString)
                                        let second = evalSet (s2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, "")
                                        {result = "    Map"+ mNum.ToString() + " : Map<" + lookAheadSet (s1, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, aNum) + ", " + lookAheadSet (s2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a) + ">\n";
                                       p = second.p;
                                       m = second.m;
                                       r = second.r;
                                       v = second.v;
                                       q = second.q;
                                       u = second.u;
                                       l = second.l;
                                       a = second.a;
                                       prepend = second.prepend + second.result + first.prepend + first.result }
    | _                         -> failwith "Error matching set in evalCartListItem"

and evalCartListSet (fListSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString, prependString) =
    match fListSet with
    | []    ->  {result = resultString;
                p = pNum;
                m = mNum;
                r = rNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                a = aNum;
                prepend = prependString}
    | a::b  ->  let prev = evalCartListItem (a, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString, prependString)
                evalCartListSet (b, prev.p, prev.m, prev.r, prev.v, prev.q, prev.u, prev.l, prev.a, resultString+prev.result, prev.prepend)


and evalSet (ast, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString) = 
    match ast with
    | CartesianListSet(fListSet)    ->  let prev = evalCartListSet (fListSet, pNum, mNum, rNum+1, vNum, qNum, uNum, lNum, aNum, resultString, "")
                                        {result = prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n";
                                        p = prev.p;
                                        m = prev.m;
                                        r = prev.r;
                                        v = prev.v;
                                        q = prev.q;
                                        u = prev.u;
                                        l = prev.l;
                                        a = prev.a;
                                        prepend = ""}
    | QSet                          ->  {result = "";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum;
                                        prepend = ""}
    | VARSet                        ->  {result = "";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum;
                                        prepend = ""}
    | ARRSet                        ->  {result = "";
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum;
                                        prepend = ""}
    | UnionSet(fSet1, fSet2)        ->  let first = evalUnionSet (fSet1, pNum, mNum, rNum, vNum, qNum, uNum+1, lNum, aNum, "", "")
                                        let second = evalUnionSet (fSet2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, "", "" )
                                        {result = first.prepend + second.prepend + "\ntype Union" + uNum.ToString() + " = \n" + first.result + second.result + "\n";
                                        p = second.p;
                                        m = second.m;
                                        r = second.r;
                                        v = second.v;
                                        q = second.q;
                                        u = second.u;
                                        l = second.l;
                                        a = second.a;
                                        prepend = ""}
    | ElemList(fList)                -> let first = evalList (fList, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, "", "")
                                        {result = first.result;
                                        p = pNum;
                                        m = mNum;
                                        r = rNum;
                                        v = vNum;
                                        q = qNum;
                                        u = uNum;
                                        l = lNum;
                                        a = aNum;
                                        prepend = ""}
    | PowersetSet (fSet)            ->  let prev = evalSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString)
                                        {result = prev.prepend + prev.result + evalPowerset(fSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum);
                                        p = prev.p;
                                        m = prev.m;
                                        r = prev.r;
                                        v = prev.v;
                                        q = prev.q;
                                        u = prev.u;
                                        l = prev.l;
                                        a = prev.a;
                                        prepend = ""}
    | TotalFunctionSpaceSet(fset1, fset2)   ->  let first = evalSet (fset1, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, aNum, resultString)
                                                let second = evalSet (fset2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, "")
                                                {result = first.result + second.result + "\ntype Map" + mNum.ToString() + " = " + "Map<" + (lookAheadSet (fset1, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum)) + "," + (lookAheadSet (fset2, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a)) + ">\n";
                                                p = second.p;
                                                m = second.m;
                                                r = second.r;
                                                v = second.v;
                                                q = second.q;
                                                u = second.u;
                                                l = second.l;
                                                a = second.a;
                                                prepend = first.prepend + second.prepend}
    | _                             -> failwith "Error detecting set at evalSet"


let rec evalDomList (fListDom, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, (resultString:string), prependString) = 
    match fListDom with
    | []    -> {result = (resultString.Remove(resultString.Length-2));  // remove extra asterisk
                p = pNum;
                m = mNum;
                r = rNum;
                v = vNum;
                q = qNum;
                u = uNum;
                l = lNum;
                a = aNum;
                prepend = prependString}
    | a::b  ->  let prev = evalDom (a, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum,  "")
                evalDomList (b, prev.p, prev.m,  prev.r, prev.v, prev.q, prev.u, prev.l, prev.a, resultString + lookAheadDomRecord (a, pNum, mNum), prev.result + prependString)

and evalDom (ast, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString) = 
    match ast with
    | PowersetDom(fSet)                     ->  let prev = evalSet (fSet, pNum+1, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString);
                                                {result = prev.result + (evalPowerset (fSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum)) + resultString;
                                                p = prev.p;
                                                m = prev.m;
                                                r = prev.r;
                                                v = prev.v;
                                                q = prev.q;
                                                u = prev.u;
                                                l = prev.l;
                                                a = prev.a;
                                                prepend = prev.prepend}
    | TotalFunctionSpaceDom(fSet, fDom)     ->  let first = evalSet(fSet, pNum, mNum+1, rNum, vNum, qNum, uNum, lNum, aNum, "")
                                                let second = evalDom(fDom, first.p, first.m, first.r, first.v, first.q, first.u, first.l, first.a, first.result)
                                                {result = second.result + "\ntype Map" + mNum.ToString() + " = " + "Map<" + (lookAheadSet (fSet, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum)) + "," + (lookAheadDom (fDom, pNum, mNum, rNum)) + ">\n"
                                                    + resultString;
                                                p = second.p;
                                                m = second.m;
                                                r = second.r;
                                                v = second.v;
                                                q = second.q;
                                                u = second.u;
                                                l = second.l;
                                                a = second.a;
                                                prepend = ""}
    | CartesianListDom(fListDom)            ->  let resDom = evalDomList (fListDom, pNum, mNum, rNum, vNum, qNum, uNum, lNum, aNum, resultString, "")
                                                {result = resDom.prepend + "\ntype ComplexDomain"+" = " + resDom.result + "\n";
                                                p = resDom.p;
                                                m = resDom.m;
                                                r = resDom.r;
                                                v = resDom.v;
                                                q = resDom.q;
                                                u = resDom.u;
                                                l = resDom.l;
                                                a = resDom.a;
                                                prepend = ""}
    | _                                     -> failwith "Error matching ast in evalDom"


let header = "[<AutoOpen>]\nmodule Domain\n\n// Generated Code Section: Domain type\n(*\nQ -> "



let format (genTypes, textForm, ast) = (header + textForm + "\n*)\n\n" 
                                        + genTypes + "\ntype sigma = " + (lookAheadDom (ast, 1, 1, 1)) + "\n\ntype AnalysisResult = Map<Node, sigma>\n")

let evaluateAST ast textForm = format ( ((evalDom (ast, 1, 1, 1, 1, 1, 1, 1, 1, "")).result ), textForm, ast )