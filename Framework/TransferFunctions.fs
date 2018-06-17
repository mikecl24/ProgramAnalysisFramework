[<AutoOpen>]
module TransferFunctions

// Graph domain Specification
// Generate a map from a list and a value to assign to
let rec genSigma xs uVal : sigma = 
    match xs with
    | []        ->  Map.empty
    | a :: b    ->  Map.fold (fun acc key value -> Map.add key value acc) (Map.empty.Add(a, uVal)) (genSigma b uVal)

let bot : sigma = genSigma Identifiers (List1 Bot)
let top : sigma = genSigma Identifiers (Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf})

printfn "Top is: %A" top
printfn "Bot is: %A" bot

// Analysis Range
let minI = -10
let maxI = 10

// Validity Check: If inside range, pass, else bottom
let chk (interval:Union1): Union1 = 
    match interval with
    | Record1 {Union2 = Int1 x; Union3 = y} when x<minI     -> List1 Bot
    | Record1 {Union2 = x; Union3 = Int2 y} when y>maxI     -> List1 Bot
    | Record1 {Union2 = Int1 x; Union3 = Int2 y} when x>y   -> List1 Bot
    | a                                                     -> a

let subset_u2 (a, b) : bool =
    match (a,b) with
    | (x, y) when x = y             -> true
    | (List2 NegInf, _)             -> false
    | (_, List2 NegInf)             -> true
    | (Int1 x, Int1 y)              -> x>y

let subset_u3 (a, b) : bool =
    match (a,b) with
    | (x, y) when x = y             -> true
    | (List3 PlusInf, _)            -> false
    | (_, List3 PlusInf)            -> true
    | (Int2 x, Int2 y)              -> x<y

let superset_u2 (a, b) : bool =
    match (a,b) with
    | (x, y) when x = y             -> true
    | (List2 NegInf, _)             -> true
    | (_, List2 NegInf)             -> false
    | (Int1 x, Int1 y)              -> x<y

let superset_u3 (a, b) : bool =
    match (a,b) with
    | (x, y) when x = y             -> true
    | (List3 PlusInf, _)            -> true
    | (_, List3 PlusInf)            -> false
    | (Int2 x, Int2 y)              -> x>y
let union_u2 (a, b) : Union2 =
    match (a,b) with
    | (x, y) when x = y             -> a
    | (List2 NegInf, _)             -> a
    | (_, List2 NegInf)             -> b
    | (Int1 x, Int1 y)  when x<y    -> a
    | (Int1 x, Int1 y)  when x>y    -> b
    | _                             -> failwith "Error: Forgotten case in union_u2"

let union_u3 (a, b) : Union3 =
    match (a,b) with
    | (x, y) when x = y             -> a
    | (List3 PlusInf, _)            -> a
    | (_, List3 PlusInf)            -> b
    | (Int2 x, Int2 y)  when x>y    -> a
    | (Int2 x, Int2 y)  when x<y    -> b
    | _                             -> failwith "Error: Forgotten case in union_u3"
 
let intersect_u2 (a, b) : Union2 =
    match (a,b) with
    | (x, y) when x = y             -> a
    | (List2 NegInf, _)             -> b
    | (_, List2 NegInf)             -> a
    | (Int1 x, Int1 y)  when x<y    -> b
    | (Int1 x, Int1 y)  when x>y    -> a
    | _                             -> failwith "Error: Forgotten case in intersect_u2"

let intersect_u3 (a, b) : Union3 =
    match (a,b) with
    | (x, y) when x = y             -> a
    | (List3 PlusInf, _)            -> b
    | (_, List3 PlusInf)            -> a
    | (Int2 x, Int2 y)  when x>y    -> b
    | (Int2 x, Int2 y)  when x<y    -> a
    | _                             -> failwith "Error: Forgotten case in intersect_u3"

// subset op for constant propagation lattice
let subset_s (set1, set2) : bool =
    match (chk(set1), chk(set2)) with
    | (x, y) when x = y                                                         -> true
    | (List1 Bot, _)                                                            -> true
    | (_, List1 Bot)                                                            -> false
    | (Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf}, _)              -> false
    | (_, Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf})              -> true
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> subset_u2 (x1, x2) && subset_u3 (y1, y2)

// superset op for constant propagation lattice
let superset_s (set1, set2) : bool = 
    match (chk(set1), chk(set2)) with
    | (x, y) when x = y                                                         -> true
    | (List1 Bot, _)                                                            -> false
    | (_, List1 Bot)                                                            -> true
    | (Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf}, _)              -> true
    | (_, Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf})              -> false
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> superset_u2 (x1, x2) && superset_u3 (y1, y2)

let union_s (set1, set2) = 
    match (chk(set1), chk(set2)) with
    | (x, y) when x = y                                                         -> set1
    | (List1 Bot, _)                                                            -> set2
    | (_, List1 Bot)                                                            -> set1
    | (Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf}, _)              -> set1
    | (_, Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf})              -> set2
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> Record1 {Union2 = union_u2 (x1, x2); Union3 = union_u3 (y1, y2)}

let intersect_s (set1, set2) = 
    match (chk(set1), chk(set2)) with
    | (x, y) when x = y                                                         -> set1
    | (List1 Bot, _)                                                            -> set1
    | (_, List1 Bot)                                                            -> set2
    | (Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf}, _)              -> set2
    | (_, Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf})              -> set1
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> Record1 {Union2 = intersect_u2 (x1, x2); Union3 = intersect_u3 (y1, y2)}

//Helper code

(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : sigma = genSigma Identifiers (Record1 {Union2 = List2 NegInf; Union3 = List3 PlusInf})
//printfn "Iota:\n%A\n" (Seq.toList iota)

//Helper code
let evalSum_u2 (a, b) = 
    match (a,b) with
    | (List2 NegInf, _)                 -> a
    | (_, List2 NegInf)                 -> b
    | (Int1 x, Int1 y) when x+y<minI    -> List2 NegInf
    | (Int1 x, Int1 y) when x+y>maxI    -> Int1 maxI
    | (Int1 x, Int1 y)                  -> Int1 (x+y) 

let evalSum_u3 (a, b) = 
    match (a,b) with
    | (List3 PlusInf, _)                -> a
    | (_, List3 PlusInf)                -> b
    | (Int2 x, Int2 y) when x+y>maxI    -> List3 PlusInf
    | (Int2 x, Int2 y) when x+y<minI    -> Int2 minI
    | (Int2 x, Int2 y)                  -> Int2 (x+y) 

let evalMin_u2 (a, b) = 
    match (a,b) with
    | (List2 NegInf, _)                 -> a
    | (_, List3 PlusInf)                -> List2 NegInf
    | (Int1 x, Int2 y) when x-y<minI    -> List2 NegInf
    | (Int1 x, Int2 y) when x-y>maxI    -> Int1 maxI
    | (Int1 x, Int2 y)                  -> Int1 (x-y) 

let evalMin_u3 (a : Union3, b : Union2) = 
    match (a,b) with
    | (Int2 x, Int1 y) when x-y>maxI    -> List3 PlusInf
    | (Int2 x, Int1 y) when x-y<minI    -> Int2 minI
    | (Int2 x, Int1 y)                  -> Int2 (x-y)
    | (List3 PlusInf, _)                -> a
    | (_, List2 NegInf)                 -> List3 PlusInf

let maxM (a, b, c, d) = 
    let i =  max (a*c) (max (a*d) (max (b*c) (b*d))) in
    match i with
    | x when x>maxI -> List3 PlusInf
    | x when x<minI -> Int2 minI
    | a             -> Int2 a

let minM (a, b, c, d) = 
    let i = min (a*c) (min (a*d) (min (b*c) (b*d))) in
    match i with
    | x when x<minI -> List2 NegInf
    | x when x>maxI -> Int1 maxI
    | a             -> Int1 a

let evalMult_u2 (a, b, c, d) = 
    match (a,b, c, d) with
    | (List2 NegInf, _, _, List3 PlusInf)                       -> List2 NegInf
    | (_, List3 PlusInf, List2 NegInf, _)                       -> List2 NegInf
    | (List2 NegInf, _, _, Int2 x) when x>0                     -> List2 NegInf
    | (Int1 x, _, _, List3 PlusInf) when x<0                    -> List2 NegInf
    | (_, List3 PlusInf, Int1 x, _) when x<0                    -> List2 NegInf
    | (_, Int2 x, List2 NegInf, _) when x>0                     -> List2 NegInf
    | (Int1 x, _, _, Int2 y) when x*y<minI                      -> List2 NegInf
    | (_, Int2 x, Int1 y, _) when x*y<minI                      -> List2 NegInf
    | (Int1 x, _, Int1 y, _) when x*y<minI                      -> List2 NegInf
    | (_, Int2 x, _, Int2 y) when x*y<minI                      -> List2 NegInf
    | (Int1 w, Int2 x, Int1 y, Int2 z)                          -> minM (w, x, y, z)
    | (List2 NegInf, Int2 x, Int1 y, Int2 z)                    -> minM (minI-1, x, y, z)
    | (Int1 w, Int2 x, List2 NegInf, Int2 z)                    -> minM (w, x, minI-1, z)
    | (Int1 w, List3 PlusInf, Int1 y, Int2 z)                   -> minM (w, maxI+1, y, z)
    | (Int1 w, Int2 x, Int1 y, List3 PlusInf)                   -> minM (w, x, y, maxI+1)
    | (List2 NegInf, List3 PlusInf, Int1 y, Int2 z)             -> minM (minI-1, maxI+1, y, z)
    | (List2 NegInf, Int2 x, List2 NegInf, Int2 z)              -> minM (minI-1, x, minI-1, z)
    | (Int1 w, List3 PlusInf, Int1 y, List3 PlusInf)            -> minM (w, maxI+1, y, maxI+1)
    | (Int1 w, Int2 x, List2 NegInf, List3 PlusInf)             -> minM (w, x, minI-1, maxI+1)

let evalMult_u3 (a , b, c, d) = 
    match (a,b, c, d) with
    | (List2 NegInf, _, List2 NegInf, _)                        -> List3 PlusInf
    | (List2 NegInf, _, Int1 x, _) when x<0                     -> List3 PlusInf
    | (Int1 x, _, List2 NegInf, _) when x<0                     -> List3 PlusInf
    | (_, List3 PlusInf, _, List3 PlusInf)                      -> List3 PlusInf
    | (_, List3 PlusInf, _, Int2 x) when x>0                    -> List3 PlusInf
    | (_, Int2 x, _, List3 PlusInf) when x>0                    -> List3 PlusInf
    | (Int1 x, _, _, Int2 y) when x*y>maxI                      -> List3 PlusInf
    | (_, Int2 x, Int1 y, _) when x*y>maxI                      -> List3 PlusInf
    | (Int1 x, _, Int1 y, _) when x*y>maxI                      -> List3 PlusInf
    | (_, Int2 x, _, Int2 y) when x*y>maxI                      -> List3 PlusInf
    | (Int1 w, Int2 x, Int1 y, Int2 z)                          -> maxM (w, x, y, z)
    | (List2 NegInf, Int2 x, Int1 y, Int2 z)                    -> maxM (minI-1, x, y, z)
    | (Int1 w, Int2 x, List2 NegInf, Int2 z)                    -> maxM (w, x, minI-1, z)
    | (Int1 w, List3 PlusInf, Int1 y, Int2 z)                   -> maxM (w, maxI+1, y, z)
    | (Int1 w, Int2 x, Int1 y, List3 PlusInf)                   -> maxM (w, x, y, maxI+1)
    | (List2 NegInf, List3 PlusInf, Int1 y, Int2 z)             -> maxM (minI-1, maxI+1, y, z)
    | (List2 NegInf, Int2 x, Int1 y, List3 PlusInf)             -> maxM (minI-1, x, y, maxI+1)
    | (Int1 w, List3 PlusInf, List2 NegInf, Int2 z)             -> maxM (w, maxI+1, minI-1, z)
    | (Int1 w, Int2 x, List2 NegInf, List3 PlusInf)             -> maxM (w, x, minI-1, maxI+1)

let maxD (a, b, c, d) = 
    let i = max (a/c) (max (a/d) (max (b/c) (b/d))) in
    match i with
    | x when x>maxI -> List3 PlusInf
    | x when x<minI -> Int2 minI
    | a             -> Int2 a

let minD (a, b, c, d) = 
    let i = min (a/c) (min (a/d) (min (b/c) (b/d))) in
    match i with
    | x when x<minI -> List2 NegInf
    | x when x>maxI -> Int1 maxI
    | a             -> Int1 a

let rec evalDiv_u3 (a, b, c, d) = 
    match (a, b, c, d) with
    | (_, _, Int1 0, _) when minI<1                             -> evalDiv_u3 (a, b, Int1 1, d)
    | (_, _, Int1 0, _) when minI>1                             -> evalDiv_u3 (a, b, Int1 minI, d)
    | (_, _, _, Int2 0) when maxI>(-1)                          -> evalDiv_u3 (a, b, c, Int2 -1)
    | (_, _, _, Int2 0) when maxI<(-1)                          -> evalDiv_u3 (a, b, c, Int2 maxI)
    | (List2 NegInf, _, List2 NegInf, _)                        -> List3 PlusInf
    | (List2 NegInf, _, Int1 y, _) when y<0                     -> List3 PlusInf
    | (_, List3 PlusInf, _, List3 PlusInf)                      -> List3 PlusInf
    | (_, List3 PlusInf, _, Int2 z) when z>0                    -> List3 PlusInf
    | (Int1 w, Int2 x, Int1 y, Int2 z)                          -> maxD (w, x, y, z)
    | (List2 NegInf, Int2 x, Int1 y, Int2 z)                    -> maxD (minI-1, x, y, z)
    | (Int1 w, List3 PlusInf, Int1 y, Int2 z)                   -> maxD (w, maxI+1, y, z)
    | (Int1 w, Int2 x, List2 NegInf, Int2 z)                    -> max (Int2 0) (maxD (w, x, minI-1, z))
    | (Int1 w, Int2 x, Int1 y, List3 PlusInf)                   -> max (Int2 0) (maxD (w, x, y, maxI+1))
    | (List2 NegInf, Int2 x, Int1 y, List3 PlusInf)             -> max (Int2 0) (maxD (minI-1, x, y, maxI+1))
    | (Int1 w, List3 PlusInf, List2 NegInf, Int2 z)             -> max (Int2 0) (maxD (w, maxI+1, minI-1, z))
    
    | (Int1 w, Int2 x, List2 NegInf, List3 PlusInf)             -> let i = max (Int2 0) (maxD (w, x, minI-1, maxI+1))
                                                                   if 1<maxI && 1>minI then
                                                                        let i2 = max (maxD (w, x, 1, 1)) i
                                                                        if -1<maxI && -1>minI then
                                                                            max (maxD (w, x, -1, -1)) i2
                                                                        else
                                                                            i2
                                                                   else
                                                                        if -1<maxI && -1>minI then
                                                                            max (maxD (w, x, -1, -1)) i
                                                                        else
                                                                            i
                                                                   
    
let rec evalDiv_u2 (a, b, c, d) = 
    match (a, b, c, d) with
    | (_, _, Int1 0, _) when minI<1                             -> evalDiv_u2 (a, b, Int1 1, d)
    | (_, _, Int1 0, _) when minI>1                             -> evalDiv_u2 (a, b, Int1 minI, d)
    | (_, _, _, Int2 0) when maxI>(-1)                          -> evalDiv_u2 (a, b, c, Int2 -1)
    | (_, _, _, Int2 0) when maxI<(-1)                          -> evalDiv_u2 (a, b, c, Int2 maxI)
    | (List2 NegInf, _, _, List3 PlusInf)                       -> List2 NegInf
    | (List2 NegInf, _, _, Int2 z) when z>0                     -> List2 NegInf
    | (_, List3 PlusInf, List2 NegInf, _)                       -> List2 NegInf
    | (_, List3 PlusInf, Int1 y, _) when y<0                    -> List2 NegInf
    | (Int1 w, Int2 x, Int1 y, Int2 z)                          -> minD (w, x, y, z)
    | (List2 NegInf, Int2 x, Int1 y, Int2 z)                    -> minD (minI-1, x, y, z)
    | (Int1 w, List3 PlusInf, Int1 y, Int2 z)                   -> minD (w, maxI+1, y, z)
    | (Int1 w, Int2 x, List2 NegInf, Int2 z)                    -> min (Int1 0) (minD (w, x, minI-1, z))
    | (Int1 w, Int2 x, Int1 y, List3 PlusInf)                   -> min (Int1 0) (minD (w, x, y, maxI+1))
    | (List2 NegInf, Int2 x, List2 NegInf, Int2 z)              -> min (Int1 0) (minD (minI-1, x, minI-1, z))
    | (Int1 w, List3 PlusInf, Int1 y, List3 PlusInf)            -> min (Int1 0) (minD (w, maxI+1, y, maxI+1))
    | (Int1 w, Int2 x, List2 NegInf, List3 PlusInf)             -> min (Int1 0) (minD (w, x, minI-1, maxI+1))




let umin_u2 (u:Union3) : Union2 = 
    match u with
    | List3 PlusInf         -> List2 NegInf
    | Int2 x when -x<minI   -> List2 NegInf
    | Int2 x when -x>maxI   -> Int1 maxI
    | Int2 x                -> Int1 -x

let umin_u3 (u:Union2) : Union3 = 
    match u with
    | List2 NegInf          -> List3 PlusInf
    | Int1 x when -x>maxI   -> List3 PlusInf
    | Int1 x when -x<minI   -> Int2 minI
    | Int1 x                -> Int2 -x

let evalSum (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Bot, _)                                                            -> List1 Bot
    | (_, List1 Bot)                                                            -> List1 Bot
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> Record1 {Union2 = evalSum_u2 (x1, x2); Union3 = evalSum_u3 (y1, y2)}
    
let evalMin (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Bot, _)                                                            -> List1 Bot
    | (_, List1 Bot)                                                            -> List1 Bot
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> Record1 {Union2 = evalMin_u2 (x1, y2); Union3 = evalMin_u3 (y1, x2)}

let evalMult (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Bot, _)                                                            -> List1 Bot
    | (_, List1 Bot)                                                            -> List1 Bot
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> Record1 {Union2 = evalMult_u2 (x1, y1, x2, y2); Union3 = evalMult_u3 (x1, y1, x2, y2)}

let evalDiv (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Bot, _)                                                            -> List1 Bot
    | (_, List1 Bot)                                                            -> List1 Bot
    | (_, Record1 {Union2 = Int1 0; Union3 = Int2 0})                           -> List1 Bot
    | (Record1 {Union2 = x1; Union3 = y1}, Record1 {Union2 = x2; Union3 = y2})  -> Record1 {Union2 = evalDiv_u2 (x1, y1, x2, y2); Union3 = evalDiv_u3 (x1, y1, x2, y2)}

let evalUmin (u : Union1) : Union1 = 
    match u with
    | List1 Bot                         -> List1 Bot
    | Record1 {Union2 = x; Union3 = y}  -> Record1 {Union2 = umin_u2 y; Union3 = umin_u3 x} 

let rec evalA (a:aexp, s:sigma) : Union1 =  
    match a with
    | VarExpr(v)            -> chk(s.[Var1 v])
    | NumExpr(i) when i<minI-> Record1 {Union2 = List2 NegInf; Union3 = Int2 minI}
    | NumExpr(i) when i>maxI-> Record1 {Union2 = Int1 maxI; Union3 = List3 PlusInf}
    | NumExpr(i)            -> Record1 {Union2 = Int1 i; Union3 = Int2 i}
    | ArrExpr(aname, aex)   -> chk(s.[Arr1 aname])
    | SumExpr(a1, a2)       -> evalSum ((evalA (a1,s)), (evalA (a2, s)))
    | MinExpr(a1, a2)       -> evalMin ((evalA (a1,s)), (evalA (a2, s)))
    | MultExpr(a1, a2)      -> evalMult ((evalA (a1,s)), (evalA (a2, s)))
    | DivExpr(a1, a2)       -> evalDiv ((evalA (a1,s)), (evalA (a2, s)))
    | UMinExpr(a)           -> evalUmin (evalA (a,s))

let getA ast = 
    match ast with
    |   AssignCommand (a, b)        -> b
    |   ArrAssignCommand(a, b, c)   -> c
    |   _                           -> failwith "Cannot extract variable from non-variable assignment"

let getVar ast = 
    match ast with
    |   AssignCommand (a, b)        -> a
    |   _                           -> failwith "Cannot extract variable from non-variable assignment"

let getArr ast = 
    match ast with
    |   ArrAssignCommand(a, b, c)   -> a
    |   _                               -> failwith "Cannot extract variable from non-array assignment"

(*            TRANSFER FUNCTIONS            *)
let TF_Boolean (inSigma : sigma, edge : Edge) : sigma = inSigma

let TF_Assignment (inSigma : sigma, edge : Edge) : sigma = inSigma.Add(Var1 (getVar edge.Action), evalA (getA edge.Action, inSigma))

let TF_Skip (inSigma : sigma, edge : Edge) : sigma = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : sigma, edge : Edge) : sigma = inSigma.Add(Arr1 (getArr edge.Action), 
                                                                                union_s (evalA (getA edge.Action, inSigma), inSigma.[Arr1 (getArr edge.Action)])
                                                                            )