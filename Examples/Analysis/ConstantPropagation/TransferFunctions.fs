[<AutoOpen>]
module TransferFunctions
open System.Runtime.InteropServices

// Graph domain Specification
// Generate a map from a list and a value to assign to
let rec genSigma xs uVal : sigma = 
    match xs with
    | []        ->  Map.empty
    | a :: b    ->  Map.fold (fun acc key value -> Map.add key value acc) (Map.empty.Add(a, uVal)) (genSigma b uVal)

let bot : sigma = genSigma Variables (List1 Bot)
let top : sigma = genSigma Variables (List1 Top)

printfn "Top is: %A" top
printfn "Bot is: %A" bot

// subset op for constant propagation lattice
let subset_s (set1, set2) : bool =
    match (set1, set2) with
    | (x, y) when x = y             -> true
    | (List1 Top, _)                -> false
    | (_, List1 Top)                -> true
    | (Int1 x, Int1 y) when x<>y    -> false
    | (List1 Bot, _)                -> true
    | (_, List1 Bot)                -> false
    | _                             -> failwith "Error: Should never happen (unless i forgot a case)"

// superset op for constant propagation lattice
let superset_s (set1, set2) : bool = 
    match (set1, set2) with
    | (x, y) when x = y             -> true
    | (List1 Bot, _)                -> false
    | (_, List1 Bot)                -> true
    | (Int1 x, Int1 y) when x<>y    -> false
    | (List1 Top, _)                -> true
    | (_, List1 Top)                -> false
    | _                             -> failwith "Error: Should never happen (unless i forgot a case)"

let union_s (set1, set2) = 
    match (set1, set2) with
    | (x, y) when x = y             -> set1
    | (List1 Bot, _)                -> set2
    | (_, List1 Bot)                -> set1
    | (Int1 x, Int1 y) when x<>y    -> List1 Top
    | (List1 Top, _)                -> set1
    | (_, List1 Top)                -> set2
    | _                             -> failwith "Error: Should never happen (unless i forgot a case)"
let intersect_s (set1, set2) = 
    match (set1, set2) with
    | (x, y) when x = y             -> set1
    | (List1 Bot, _)                -> set1
    | (_, List1 Bot)                -> set2
    | (Int1 x, Int1 y) when x<>y    -> List1 Bot
    | (List1 Top, _)                -> set2
    | (_, List1 Top)                -> set1
    | _                             -> failwith "Error: Should never happen (unless i forgot a case)"

//Helper code

(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : sigma = genSigma Variables (List1 Top)
//printfn "Iota:\n%A\n" (Seq.toList iota)

//Helper code
let evalSum (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Top, List1 Top)    ->  List1 Top
    | (List1 Top, List1 Bot)    ->  List1 Bot
    | (List1 Top, Int1 y)       ->  List1 Top
    | (List1 Bot, List1 Top)    ->  List1 Bot
    | (List1 Bot, List1 Bot)    ->  List1 Bot
    | (List1 Bot, Int1 y)       ->  List1 Bot
    | (Int1 x, List1 Top)       ->  List1 Top
    | (Int1 x, List1 Bot)       ->  List1 Bot
    | (Int1 x, Int1 y)          ->  Int1 (x+y)

let evalMin (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Top, List1 Top)    ->  List1 Top
    | (List1 Top, List1 Bot)    ->  List1 Bot
    | (List1 Top, Int1 y)       ->  List1 Top
    | (List1 Bot, List1 Top)    ->  List1 Bot
    | (List1 Bot, List1 Bot)    ->  List1 Bot
    | (List1 Bot, Int1 y)       ->  List1 Bot
    | (Int1 x, List1 Top)       ->  List1 Top
    | (Int1 x, List1 Bot)       ->  List1 Bot
    | (Int1 x, Int1 y)          ->  Int1 (x-y)

let evalMult (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Top, List1 Top)    ->  List1 Top
    | (List1 Top, List1 Bot)    ->  List1 Bot
    | (List1 Top, Int1 y)       ->  List1 Top
    | (List1 Bot, List1 Top)    ->  List1 Bot
    | (List1 Bot, List1 Bot)    ->  List1 Bot
    | (List1 Bot, Int1 y)       ->  List1 Bot
    | (Int1 x, List1 Top)       ->  List1 Top
    | (Int1 x, List1 Bot)       ->  List1 Bot
    | (Int1 x, Int1 y)          ->  Int1 (x*y)

let evalDiv (u1 : Union1, u2 : Union1) : Union1 = 
    match (u1, u2) with
    | (List1 Top, List1 Top)    ->  List1 Top
    | (List1 Top, List1 Bot)    ->  List1 Bot
    | (List1 Top, Int1 y)       ->  List1 Top
    | (List1 Bot, List1 Top)    ->  List1 Bot
    | (List1 Bot, List1 Bot)    ->  List1 Bot
    | (List1 Bot, Int1 y)       ->  List1 Bot
    | (Int1 x, List1 Top)       ->  List1 Top
    | (Int1 x, List1 Bot)       ->  List1 Bot
    | (Int1 x, Int1 y) when y=0 ->  List1 Top   // division by 0 will cancel flow of constants
    | (Int1 x, Int1 y)          ->  Int1 (x/y)

let evalUmin (u : Union1) : Union1 = 
    match u with
    | List1 Top    ->  List1 Top
    | List1 Bot    ->  List1 Bot
    | Int1 x       ->  Int1 (-x)
let rec evalA (a:aexp, s:sigma) : Union1 =  
    match a with
    | VarExpr(v)            -> s.[v]
    | NumExpr(i)            -> Int1 i
    | ArrExpr(aname, aex)   -> List1 Top
    | SumExpr(a1, a2)       -> evalSum ((evalA (a1,s)), (evalA (a2, s)))
    | MinExpr(a1, a2)       -> evalMin ((evalA (a1,s)), (evalA (a2, s)))
    | MultExpr(a1, a2)      -> evalMult ((evalA (a1,s)), (evalA (a2, s)))
    | DivExpr(a1, a2)       -> evalDiv ((evalA (a1,s)), (evalA (a2, s)))
    | UMinExpr(a)           -> evalUmin (evalA (a,s))

let getA ast = 
    match ast with
    |   AssignCommand (a, b)         -> b
    |   _                           -> failwith "Cannot extract variable from non-variable assignment"

let getVar ast = 
    match ast with
    |   AssignCommand (a, b)         -> a
    |   _                           -> failwith "Cannot extract variable from non-variable assignment"

let getArr ast = 
    match ast with
    |   ArrAssignCommand(a, b, c)   -> a
    |   _                               -> failwith "Cannot extract variable from non-array assignment"

(*            TRANSFER FUNCTIONS            *)
let TF_Boolean (inSigma : sigma, edge : Edge) : sigma = inSigma

let TF_Assignment (inSigma : sigma, edge : Edge) : sigma = inSigma.Add(getVar edge.Action, evalA (getA edge.Action, inSigma))

let TF_Skip (inSigma : sigma, edge : Edge) : sigma = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : sigma, edge : Edge) : sigma = inSigma