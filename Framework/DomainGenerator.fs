[<AutoOpen>]
module DomainGenerator

// Type to return information between functions: Need to know updated fresh numbers
type DomainGenType = {
    result : string;    // Result string
    p : int;            // Powerset
    m : int;            // Map
    q : int;            // Node
    v : int;            // Variable
    a : int;            // Array
    id : int;           // Identifiers
    r : int;            // Record
    u : int;            // Union
    l : int;            // Lists
    i : int;            // ints
    s : int;            // Strings
    prepend : string    // Any string that might have to be prepended
    mD : Map<int, string*string*string> // Metadata about maps for generating
}

// Variable containing the enviroment variables in this program
let getDescriptor (ast : sset) : string = 
    match ast with
    | QSet      -> if not (Nodes.Length = 0) then "Nodes"
                    else failwith "Error: Using Q in an empty program"
    | VARSet    -> if not (Variables.Length = 0) then "Variables"
                    else failwith "Error: Using VAR in a program without any variables"
    | ARRSet    -> if not (Arrays.Length = 0) then "Arrays"
                    else failwith "Error: Using ARR in a program without any arrays"
    | IDENTSet  -> if not (Identifiers.Length = 0) then "Identifiers"
                    else failwith "Using IDENT in a program without any variables or arrays"

// Create the string for a powerset
let powersetString pNum next : string= ("\ntype Powerset" + (pNum.ToString()) +  " = " + next + " Set\n")

// Get the equivalent string from a SSet expression
let evalSSet (typeNext : sset) : string = 
    match typeNext with
    | QSet      -> if not (Nodes.Length = 0) then "Node"
                    else failwith "Error: Using Q in an empty program"
    | VARSet    -> if not (Variables.Length = 0) then "Var"
                    else failwith "Error: Using VAR in a program without any variables"
    | ARRSet    -> if not (Arrays.Length = 0) then "Arr"
                    else failwith "Error: Using ARR in a program without any arrays"
    | IDENTSet  -> if not (Identifiers.Length = 0) then "Ident"
                    else failwith "Using IDENT in a program without any variables or arrays"

// Generate the string for a powerset of any set
let evalPowerset (typeNext : set, pNum, mNum, rNum, uNum, lNum) : string =
    match typeNext with
    | SsetSet(ftype)                -> (powersetString pNum (evalSSet ftype))
    | CartesianSet(_)               -> failwith "Error: Attempting to process non-consolidated domain (Cartesian Set)"
    | CartesianListSet(fListSet)    -> (powersetString pNum ("Record" +  rNum.ToString()))
    | TotalFunctionSpaceSet(s1, s2) -> (powersetString pNum ("Map" +  (mNum).ToString()))
    | PowersetSet(fSet)             -> (powersetString pNum ("Powerset" +  (pNum+1).ToString()))
    | UnionSet(fSet1, fSet2)        -> (powersetString pNum ("Union" +  uNum.ToString()))
    | ListSet(_)                    -> failwith "Error: Attempting to process non-consolidated domain (List Set)"
    | ElemList(fList)               -> (powersetString pNum ("List" +  lNum.ToString()))
    | INTSet                        -> (powersetString pNum ("Int"))
    | STRSet                        -> (powersetString pNum ("Str"))

// Look ahead in a set to find the type name
let lookAheadSet (ast : set, p, m, r, u, l) : string = 
    match ast with
    | SsetSet(ftype)                        -> evalSSet ftype
    | CartesianSet(_)                       -> failwith "Error: Attempting to process non-consolidated domain in lookahead (Cartesian Set)"
    | CartesianListSet(fListSet)            -> "Record"+r.ToString()
    | TotalFunctionSpaceSet(s1, s2)         -> "Map"+m.ToString()
    | PowersetSet(fSet)                     -> "Powerset"+p.ToString()
    | UnionSet(fSet1, fSet2)                -> "Union"+u.ToString()
    | ListSet(_)                            -> failwith "Error: Attempting to process non-consolidated domain in lookahed (List Set)"
    | ElemList(fList)                       -> "List"+l.ToString()
    | INTSet                                -> "Int"
    | STRSet                                -> "Str"

// Look ahead in a domain to find the type name
let lookAheadDom (ast : domain, p, m, r, u, l) : string = 
    match ast with
    | PowersetDom(fSet)                     -> "Powerset"+p.ToString()
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map"+m.ToString()
    | CartesianDom(_)                       -> failwith "Error: Attempting to process non-consolidated domain (Cartesian Domain)"
    | CartesianListDom(fListDom)            -> "ComplexDomain"
    | SetDom(s)                             -> lookAheadSet (s, p, m, r, u, l)

// Lookahed in domain in a Cartesian domain to form the tuple description string
let lookAheadDomRecord (ast : domain, p, m, r, u, l) =
    match ast with
    | PowersetDom(fSet)                     -> "Powerset" + p.ToString() + " * "
    | TotalFunctionSpaceDom(fSet, fDom)     -> "Map" + m.ToString() + " * "
    | CartesianDom(_)                       -> failwith "Error: Lookahead on a non-consolidated Cartesian domain"
    | CartesianListDom(_)                   -> failwith "Error: Lookahead on a cartesian domain (should not happen in recursion)"
    | SetDom(s)                             -> failwith "Error: Cartesian Domain with an unsafe domain (Not allowed by specification)"
                                                // lookAheadSet (s, p, m, r, u, l) + " * " // if cartesian unsafe domains are allowed

// Return the type of a list type with discr. union cases in a string list
let rec evalList (fList : string list, lNum, result) : string =
    match fList with
    | []    ->  "\ntype List" + lNum.ToString() + " =\n" + result + "\n";
    | a::b  ->  evalList (b, lNum, (result + "    | " + a + "\n" ))

// Get the variable string from a SSet expression
let evalSsetVar (typeNext : sset, qNum, vNum, aNum, idNum) : string = 
    match typeNext with
    | QSet                          -> "Node" + qNum.ToString()
    | VARSet                        -> "Var" + vNum.ToString()
    | ARRSet                        -> "Arr" + aNum.ToString()
    | IDENTSet                      -> "Ident" + idNum.ToString()

// Increment Number for SSet
let ssetIncr (typeNext : sset, typeNr) : int=
    match typeNext with
    | QSet when typeNr = "q"        -> 1
    | VARSet when typeNr = "v"      -> 1
    | ARRSet when typeNr = "a"      -> 1
    | IDENTSet when typeNr = "id"   -> 1
    | _                             -> 0

// Return string for case in union statement
let rec evalUnionSet (fSet : set, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum) : DomainGenType =
    match fSet with
    | SsetSet(ftype)                ->  {result = "    | " + (evalSsetVar (ftype, qNum, vNum, aNum, idNum)) + " of " + (evalSSet ftype) + " \n";
                                        q = qNum + ssetIncr (ftype, "q");
                                        v = vNum + ssetIncr (ftype, "v");
                                        a = aNum + ssetIncr (ftype, "a");
                                        id = idNum + ssetIncr (ftype, "id");
                                        // No change
                                        p = pNum; m = mNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum; prepend = ""; mD = Map.empty
                                        }
    | CartesianSet(_)               ->  failwith "Error: Attempting to process non-consolidated domain in Union (Cartesian Set)"
    | CartesianListSet(fListSet)    ->  let prev = evalCartListSet (fListSet, pNum, mNum, qNum, vNum, aNum, idNum, rNum+1, uNum, lNum, iNum, sNum,  "", "", Map.empty)
                                        {result = "    | Record" + rNum.ToString() + " of Record" + rNum.ToString() + "\n";
                                        prepend =  prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n"; // prepend the type needed
                                        // No change
                                        p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; mD = prev.mD
                                        }
    | TotalFunctionSpaceSet(s1, s2) ->  let prev = evalSet (s2, pNum, mNum+1, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                        {result = "    | Map" + mNum.ToString() + " of Map<" + evalSSet (s1) + ", " + lookAheadSet (s2, pNum, mNum+1, rNum, uNum, lNum) + "> \n";
                                        prepend =  prev.prepend + prev.result + "\n";   // second type evaluation
                                        mD = (prev.mD).Add(mNum, (getDescriptor s1, evalSSet s1, lookAheadSet (s2, pNum, mNum+1, rNum, uNum, lNum)));
                                        // From domain evaluation
                                        p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s
                                        }
    | PowersetSet(fSet)             ->  let prev = evalSet (fSet, pNum+1, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                        {result = "    | Powerset" + pNum.ToString() + " of " + lookAheadSet (fSet, pNum+1, mNum, rNum, uNum, lNum) + " Set \n";
                                        prepend =  prev.prepend + prev.result + "\n";
                                        // From domain evaluation
                                        p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; mD = prev.mD
                                        }
    | UnionSet(fSet1, fSet2)        ->  let first = evalUnionSet (fSet1, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum+1, lNum, iNum, sNum)
                                        let second = evalUnionSet (fSet2, first.p, first.m, first.q, first.v, first.a, first.id, first.r, first.u, first.l, first.i, first.s)
                                        {result = "    | Union" + uNum.ToString() + " of Union" + uNum.ToString();
                                        prepend = first.prepend + second.prepend + "\ntype Union" + uNum.ToString() + " =\n" + first.result + second.result + "\n";
                                        mD = Map.fold (fun acc key value -> Map.add key value acc) first.mD second.mD;
                                        // From domain evaluations
                                        p = second.p; m = second.m; q = second.q; v = second.v; a = second.a; id = second.id; r = second.r; u = second.u; l = second.l; i = second.i; s = second.s
                                        }
    | ListSet(_)                    ->  failwith "Error: Attempting to process non-consolidated domain in Union (List Set)"
    | ElemList(fList)               ->  let listRes = evalList (fList, lNum, "")
                                        {result = "    | List" + lNum.ToString() + " of List" + lNum.ToString() + "\n";
                                        l = lNum+1;
                                        prepend = listRes;
                                        // No change
                                        p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; i = iNum; s = sNum; mD = Map.empty
                                        }
    | INTSet                        ->  {result = "    | Int" + iNum.ToString() + " of int \n";
                                        i = iNum + 1;
                                        // No change
                                        p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; s = sNum; prepend = ""; mD = Map.empty
                                        }
    | STRSet                        ->  {result = "    | Str" + sNum.ToString() + " of string \n";
                                        s = sNum+1;
                                        // No change
                                        p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; prepend = ""; mD = Map.empty
                                        }

// Return string for cartesian set item (record context)
and evalCartListItem (ast : set, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum) : DomainGenType =
    match ast with
    | SsetSet(ftype)                 -> {result = "    " + (evalSsetVar (ftype, qNum, vNum, aNum, idNum)) + " : " + (evalSSet ftype) + " ;\n";
                                        q = qNum + ssetIncr (ftype, "q");
                                        v = vNum + ssetIncr (ftype, "v");
                                        a = aNum + ssetIncr (ftype, "a");
                                        id = idNum + ssetIncr (ftype, "id");
                                        // No change
                                        p = pNum; m = mNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum; prepend = ""; mD = Map.empty
                                        }
    | CartesianSet(_)               ->  failwith "Error: Attempting to process non-consolidated domain in Cartesian Set (Cartesian Set)"
    | CartesianListSet(_)           ->  failwith "Error: Attempting to process non-consolidated domain in Cartesian Set (Cartesian List Set)"
    | TotalFunctionSpaceSet(s1, s2) ->  let prev = evalSet (s2, pNum, mNum+1, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                        {result = "    Map" + mNum.ToString() + " : Map" + mNum.ToString() + ";\n";
                                        prepend =  prev.prepend + prev.result + "\ntype Map" + mNum.ToString() + " = Map<" + evalSSet (s1) + ", " + lookAheadSet (s2, pNum, mNum+1, rNum, uNum, lNum) + ">\n";   // second type evaluation
                                        mD = (prev.mD).Add(mNum, (getDescriptor s1, evalSSet s1, lookAheadSet (s2, pNum, mNum+1, rNum, uNum, lNum)));
                                        // From domain evaluation
                                        p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s
                                        }
    | PowersetSet(fSet)             ->  let prev = evalSet (fSet, pNum+1, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                        {result = "    Powerset"  + pNum.ToString() + " : " + lookAheadSet (fSet, pNum+1, mNum, rNum, uNum, lNum) + " Set;\n";
                                        prepend =  prev.prepend + prev.result + "\n";
                                        // From domain evaluation
                                        p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; mD = prev.mD
                                        }
    | UnionSet(fSet1, fSet2)        ->  let first = evalUnionSet (fSet1, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum+1, lNum, iNum, sNum)
                                        let second = evalUnionSet (fSet2, first.p, first.m, first.q, first.v, first.a, first.id, first.r, first.u, first.l, first.i, first.s)
                                        {result = "    Union" + uNum.ToString() + " : Union" + uNum.ToString() + ";\n";
                                        prepend = first.prepend + second.prepend + "\ntype Union" + uNum.ToString() + " =\n" + first.result + second.result + "\n";
                                        mD = Map.fold (fun acc key value -> Map.add key value acc) first.mD second.mD;
                                        // From domain evaluations
                                        p = second.p; m = second.m; q = second.q; v = second.v; a = second.a; id = second.id; r = second.r; u = second.u; l = second.l; i = second.i; s = second.s
                                        }
    | ListSet(_)                    ->  failwith "Error: Attempting to process non-consolidated domain in Cartesian Set (List Set)"
    | ElemList(fList)               -> let listRes = evalList (fList, lNum, "")
                                       {result = "    List"  + lNum.ToString() + " : List" + lNum.ToString() + ";\n";
                                       l = lNum+1;
                                       prepend = listRes;
                                       // No change
                                       p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; i = iNum; s = sNum; mD = Map.empty
                                       }
    | INTSet                        ->  {result = "    Int" + iNum.ToString() + " : int ;\n";
                                        i = iNum + 1;
                                        // No change
                                        p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; s = sNum; prepend = ""; mD = Map.empty
                                        }
    | STRSet                        ->  {result = "    Str" + sNum.ToString() + " : string ;\n";
                                        s = sNum+1;
                                        // No change
                                        p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; prepend = ""; mD = Map.empty
                                        }

// Evaluate a cartesian set list (process them individually and append)
and evalCartListSet (cart : set list, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum, resultString, prependString, oldMD) : DomainGenType =
    match cart with
    | []    ->  {result = resultString;
                p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum;
                prepend = prependString; mD = oldMD}
    | a::b  ->  let prev = evalCartListItem (a, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                evalCartListSet (b, prev.p, prev.m, prev.q, prev.v, prev.a, prev.id, prev.r, prev.u, prev.l, prev.i, prev.s, resultString+prev.result, prependString + prev.prepend, (Map.fold (fun acc key value -> Map.add key value acc) prev.mD oldMD))

// Evaluate a Set to get the type structure
and evalSet (ast : set, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum) : DomainGenType =
    match ast with
    | SsetSet(ftype)                    ->  // No change
                                            {result = ""; p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum; prepend = ""; mD = Map.empty}
    | CartesianSet(_)                   ->  failwith "Error: Attempting to process non-consolidated domain in Set (Cartesian Set)"
    | CartesianListSet(fListSet)        ->  let prev = evalCartListSet (fListSet, pNum, mNum, qNum, vNum, aNum, idNum, rNum+1, uNum, lNum, iNum, sNum, "", "", Map.empty)
                                            {result = prev.prepend + "\ntype Record" + rNum.ToString() + " = {\n" + prev.result+"}\n";
                                            // From domain evaluations
                                            p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; prepend = ""; mD = prev.mD
                                            }
    | TotalFunctionSpaceSet(s1, s2)     ->  let prev = evalSet (s2, pNum, mNum+1, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                            {result = prev.result + "\ntype Map" + mNum.ToString() + " = " + "Map<" + evalSSet (s1) + "," + lookAheadSet (s2, pNum, mNum+1, rNum, uNum, lNum) + ">\n";
                                            mD = (prev.mD).Add(mNum, (getDescriptor s1, evalSSet s1, lookAheadSet (s2, pNum, mNum+1, rNum, uNum, lNum)));
                                            // From domain evaluation
                                            p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; prepend = ""
                                            }
    | PowersetSet (fSet)                ->  let prev = evalSet (fSet, pNum+1, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                            {result = prev.result + evalPowerset(fSet, pNum, mNum, rNum, uNum, lNum);
                                            // From domain evaluation
                                            p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; prepend = ""; mD = prev.mD
                                            }
    | UnionSet(fSet1, fSet2)            ->  let first = evalUnionSet (fSet1, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum+1, lNum, iNum, sNum)
                                            let second = evalUnionSet (fSet2, first.p, first.m, first.q, first.v, first.a, first.id, first.r, first.u, first.l, first.i, first.s)
                                            {result = first.prepend + second.prepend + "\ntype Union" + uNum.ToString() + " = \n" + first.result + second.result + "\n";
                                            mD = Map.fold (fun acc key value -> Map.add key value acc) first.mD second.mD;
                                            // From domain evaluations
                                            p = second.p; m = second.m; q = second.q; v = second.v; a = second.a; id = second.id; r = second.r; u = second.u; l = second.l; i = second.i; s = second.s; prepend = ""
                                            }
    | ListSet(_)                        ->  failwith "Error: Attempting to process non-consolidated domain in Set (List Set)"
    | ElemList(fList)                   ->  let listRes = evalList (fList, lNum, "")
                                            {result = listRes;
                                            l = lNum+1;
                                            // No change
                                            p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; i = iNum; s = sNum; prepend = ""; mD = Map.empty
                                            }
    | INTSet                            ->  // No change
                                            {result = ""; p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum; prepend = ""; mD = Map.empty}
    | STRSet                            ->  // No change
                                            {result = ""; p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum; prepend = ""; mD = Map.empty}

// Evaluate a list of domains to get the type structure
let rec evalDomList (fListDom : domain List, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum, (result:string), prependString, oldMD) : DomainGenType = 
    match fListDom with
    | []    -> {result = (result.Remove(result.Length-2));  // remove extra asterisk
                p = pNum; m = mNum; q = qNum; v = vNum; a = aNum; id = idNum; r = rNum; u = uNum; l = lNum; i = iNum; s = sNum;
                prepend = prependString; mD = oldMD}
    | a::b  ->  let prev = evalDom (a, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                evalDomList (b, prev.p, prev.m, prev.q, prev.v, prev.a, prev.id, prev.r, prev.u, prev.l, prev.i, prev.s, result + lookAheadDomRecord (a, pNum, mNum, rNum, uNum, lNum), prev.result + prependString, (Map.fold (fun acc key value -> Map.add key value acc) prev.mD oldMD))

//Evaluate a domain to get the type structure
and evalDom (ast : domain, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum) : DomainGenType = 
    match ast with
    | PowersetDom(fSet)                     ->  let prev = evalSet (fSet, pNum+1, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum);
                                                {result = prev.result + (evalPowerset (fSet, pNum, mNum, rNum, uNum, lNum));
                                                // From set evaluation
                                                p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; prepend = ""; mD = prev.mD
                                                }
    | TotalFunctionSpaceDom(sSet, fDom)     ->  let prev = evalDom (fDom, pNum, mNum+1, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                                {result = prev.result + "\ntype Map" + mNum.ToString() + " = " + "Map<" + (evalSSet sSet) + "," + (lookAheadDom (fDom, pNum, mNum, rNum, uNum, lNum)) + ">\n";
                                                mD = (prev.mD).Add(mNum, (getDescriptor sSet, evalSSet sSet, lookAheadDom (fDom, pNum, mNum, rNum, uNum, lNum)));
                                                // From domain evaluation
                                                p = prev.p; m = prev.m; q = prev.q; v = prev.v; a = prev.a; id = prev.id; r = prev.r; u = prev.u; l = prev.l; i = prev.i; s = prev.s; prepend = ""
                                                }
    | CartesianDom(_)                       ->  failwith "Error: Attempting to process non-consolidated domain in Domain (Cartesian Dom)"
    | CartesianListDom(fListDom)            ->  let resDom = evalDomList (fListDom, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum, "", "", Map.empty)
                                                {result = resDom.prepend + "\ntype ComplexDomain"+" = " + resDom.result + "\n";
                                                // From domain evaluations
                                                p = resDom.p; m = resDom.m; q = resDom.q; v = resDom.v; a = resDom.a; id = resDom.id; r = resDom.r; u = resDom.u; l = resDom.l; i = resDom.i; s = resDom.s; prepend = ""; mD = resDom.mD
                                                }
    | SetDom(s)                             ->  let resSet = evalSet (s, pNum, mNum, qNum, vNum, aNum, idNum, rNum, uNum, lNum, iNum, sNum)
                                                {result = resSet.result;
                                                // From set evaluation
                                                p = resSet.p; m = resSet.m; q = resSet.q; v = resSet.v; a = resSet.a; id = resSet.id; r = resSet.r; u = resSet.u; l = resSet.l; i = resSet.i; s = resSet.s; prepend = ""; mD = resSet.mD
                                                }

// Header string in every domain (module + standard comment)
let header = "[<AutoOpen>]\nmodule Domain\n\n// Generated Code Section: Domain type\n(*\nQ -> "

// Assemble the F# file from the parts
let format (genTypes, textForm, ast) = (header + textForm + "\n*)\n\n" 
                                        + genTypes + "\ntype sigma = " + (lookAheadDom (ast, 1, 1, 1, 1, 1)) + "\n\ntype AnalysisResult = Map<Node, sigma>\n")

// Function to create type string from ast and text of domain
let evaluateAST (ast : domain) (textForm: string) : string * int * Map<int, string*string*string> = 
    let i =  (evalDom (ast, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1)) in
    ((format (i.result, textForm, ast)), i.m, i.mD)