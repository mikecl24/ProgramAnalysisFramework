[<AutoOpen>]
module TransferFunctions
open System.Runtime.InteropServices

//Helper code
let iotaValue = Set.empty.Add(Plus).Add(Zero).Add(Minus)
let initValue = Set.empty
let rec genIotaV (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genIotaV (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(Var2 var, iotaValue))))


let rec genIotaA (arrs, oldIota) =
    match arrs with
    | [] -> oldIota
    | arr::next -> genIotaA (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(Arr2 arr, iotaValue))))

let rec genInitV (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genInitV (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(Var2 var, initValue))))


let rec genInitA (arrs, oldIota) =
    match arrs with
    | [] -> oldIota
    | arr::next -> genInitA (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(Arr2 arr, initValue))))


(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : sigma = (Map.fold (fun acc key value -> Map.add key value acc) (genIotaV (Variables, Map.empty)) (genIotaA (Arrays, Map.empty)))
//printfn "Iota:\n%A\n" iota
// Init
let init : sigma = (Map.fold (fun acc key value -> Map.add key value acc) (genInitV (Variables, Map.empty)) (genInitA (Arrays, Map.empty)))
//printfn "Init:\n%A\n" init
 

//Helper code
let beta (n:int) = 
    match n with
    | x when x>0    -> Set.empty.Add(Plus)
    | x when x=0    -> Set.empty.Add(Zero)
    | x when x<0    -> Set.empty.Add(Minus)
    | _             -> failwith "Not a valid integer"

let rec setUnionList (lis : Powerset1 list): Powerset1 = 
    match lis with
    | []    -> Set.empty
    | a::b  -> Set.union a (setUnionList b)

let evalSum3 (a:List1, b:List1) : Powerset1 =
    match (a,b) with
    | (Plus, Plus)  ->  Set.empty.Add(Plus)
    | (Plus, Zero)  ->  Set.empty.Add(Plus)
    | (Plus, Minus) ->  Set.empty.Add(Plus).Add(Zero).Add(Minus)
    | (Zero, Plus)  ->  Set.empty.Add(Plus)
    | (Zero, Zero)  ->  Set.empty.Add(Zero)
    | (Zero, Minus) ->  Set.empty.Add(Minus)
    | (Minus, Plus) ->  Set.empty.Add(Plus).Add(Zero).Add(Minus)
    | (Minus, Zero) ->  Set.empty.Add(Minus)
    | (Minus, Minus)->  Set.empty.Add(Minus)

let evalSum2 (a:List1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalSum3 (a,x)) b
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let evalSum1 (a:Powerset1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalSum2 (x,b)) a 
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let evalMin3 (a:List1, b:List1) : Powerset1 =
    match (a,b) with
    | (Plus, Plus)  ->  Set.empty.Add(Plus).Add(Zero).Add(Minus)
    | (Plus, Zero)  ->  Set.empty.Add(Plus)
    | (Plus, Minus) ->  Set.empty.Add(Plus)
    | (Zero, Plus)  ->  Set.empty.Add(Minus)
    | (Zero, Zero)  ->  Set.empty.Add(Zero)
    | (Zero, Minus) ->  Set.empty.Add(Plus)
    | (Minus, Plus) ->  Set.empty.Add(Minus)
    | (Minus, Zero) ->  Set.empty.Add(Minus)
    | (Minus, Minus)->  Set.empty.Add(Plus).Add(Zero).Add(Minus)

let evalMin2 (a:List1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalMin3 (a,x)) b
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let evalMin1 (a:Powerset1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalMin2 (x,b)) a 
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList


let evalMult3 (a:List1, b:List1) : Powerset1 =
    match (a,b) with
    | (Plus, Plus)  ->  Set.empty.Add(Plus)
    | (Plus, Zero)  ->  Set.empty.Add(Zero)
    | (Plus, Minus) ->  Set.empty.Add(Minus)
    | (Zero, Plus)  ->  Set.empty.Add(Zero)
    | (Zero, Zero)  ->  Set.empty.Add(Zero)
    | (Zero, Minus) ->  Set.empty.Add(Zero)
    | (Minus, Plus) ->  Set.empty.Add(Minus)
    | (Minus, Zero) ->  Set.empty.Add(Zero)
    | (Minus, Minus)->  Set.empty.Add(Plus)

let evalMult2 (a:List1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalMult3 (a,x)) b
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let evalMult1 (a:Powerset1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalMult2 (x,b)) a 
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList


let evalDiv3 (a:List1, b:List1) : Powerset1 =
    match (a,b) with
    | (Plus, Plus)  ->  Set.empty.Add(Plus)
    | (Plus, Zero)  ->  Set.empty
    | (Plus, Minus) ->  Set.empty.Add(Minus)
    | (Zero, Plus)  ->  Set.empty.Add(Zero)
    | (Zero, Zero)  ->  Set.empty
    | (Zero, Minus) ->  Set.empty.Add(Zero)
    | (Minus, Plus) ->  Set.empty.Add(Minus)
    | (Minus, Zero) ->  Set.empty
    | (Minus, Minus)->  Set.empty.Add(Plus)

let evalDiv2 (a:List1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalDiv3 (a,x)) b
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let evalDiv1 (a:Powerset1, b:Powerset1) : Powerset1 =
    Set.map (fun x -> evalDiv2 (x,b)) a 
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let evalUmin2 (a:List1) : Powerset1 =
    match a with
    | Plus  ->  Set.empty.Add(Minus)
    | Zero  ->  Set.empty.Add(Zero)
    | Minus ->  Set.empty.Add(Plus)

let evalUmin1 (a:Powerset1) : Powerset1 =
    Set.map (fun x -> evalUmin2 (x)) a 
    |> Set.fold (fun l se -> se::l) [] 
    |> setUnionList

let rec evalA (a:aexp, s:sigma) : Powerset1 =  
    match a with
    | VarExpr(v)            -> s.[Var2 v]
    | NumExpr(i)            -> beta i
    | ArrExpr(aname, aex)   -> s.[Arr2 aname]
    | SumExpr(a1, a2)       -> evalSum1 ((evalA (a1,s)), (evalA (a2, s)))
    | MinExpr(a1, a2)       -> evalMin1 ((evalA (a1,s)), (evalA (a2, s)))
    | MultExpr(a1, a2)      -> evalMult1 ((evalA (a1,s)), (evalA (a2, s)))
    | DivExpr(a1, a2)       -> evalDiv1 ((evalA (a1,s)), (evalA (a2, s)))
    | UMinExpr(a)           -> evalUmin1 (evalA (a,s))


let getVar ast = 
    match ast with
    |   AssignCommand (a, b)         -> a
    |   _                           -> failwith "Cannot extract variable from non-variable assignment"

let getA ast = 
    match ast with
    |   AssignCommand (a, b)         -> b
    |   _                           -> failwith "Cannot extract variable from non-variable assignment"

let getArr ast = 
    match ast with
    |   ArrAssignCommand(a, b, c)   -> a
    |   _                               -> failwith "Cannot extract variable from non-array assignment"

(*            TRANSFER FUNCTIONS            *)
let TF_Boolean (inSigma : sigma, edge : Edge) : sigma = inSigma

let TF_Assignment (inSigma : sigma, edge : Edge) : sigma = 
    inSigma.Add(Var2 (getVar edge.Action), evalA (getA edge.Action, inSigma))
    //(Map.fold (fun acc key value -> Map.add key value acc) inSigma (Map.empty.Add(Var1 (getVar edge.Action), Set.empty.Add(Record1 {Q1 = edge.Q2; Q2 = edge.Q1}))))
    
let TF_Skip (inSigma : sigma, edge : Edge) : sigma = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : sigma, edge : Edge) : sigma = 
    inSigma
    // Can't kill, and is already top, so no action is done!