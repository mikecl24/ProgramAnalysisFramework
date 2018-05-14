[<AutoOpen>]
module TransferFunctions
open System.Runtime.InteropServices

//Helper code
let rec genIotaV (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genIotaV (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(var, Set.empty.Add(List1(QM1))))))


let rec genIotaA (arrs, oldIota) =
    match arrs with
    | [] -> oldIota
    | arr::next -> genIotaA (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(arr, Set.empty.Add(List2(QM2))))))

let rec genInitV (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genInitV (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(var, Set.empty))))


let rec genInitA (arrs, oldIota) =
    match arrs with
    | [] -> oldIota
    | arr::next -> genInitA (next, (Map.fold (fun acc key value -> Map.add key value acc) oldIota (Map.empty.Add(arr, Set.empty))))
(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : sigma = ((genIotaV (Variables, Map.empty)), (genIotaA (Arrays, Map.empty)))
//printfn "Iota:\n%A\n" iota
// Init
let init : sigma = ((genInitV (Variables, Map.empty)), (genInitA (Arrays, Map.empty)))
//printfn "Iota:\n%A\n" init


//Helper code
// let rec remove (inSet, outSet, killCond) = 
//     match inSet with
//     | [] -> outSet
//     | x::xs when x.Var1=killCond -> remove (xs, outSet, killCond)
//     | x::xs -> remove (xs, (Set.union outSet (Set.empty.Add(x))), killCond)

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

let TF_Assignment (inSigma : sigma, edge : Edge) : sigma = 
    ((Map.fold (fun acc key value -> Map.add key value acc) (fst inSigma) (Map.empty.Add((getVar edge.Action), Set.empty.Add(Record1 {Q1 = edge.Q2; Q2 = edge.Q1})))), 
        (snd inSigma))
    
let TF_Skip (inSigma : sigma, edge : Edge) : sigma = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : sigma, edge : Edge) : sigma = 
    ((fst inSigma),
        (Map.fold (fun acc key value -> Map.add key value acc) (snd inSigma) (Map.empty.Add((getArr edge.Action), ((Set.union (snd inSigma).[(getArr edge.Action)]) (Set.empty.Add(Record2 {Q3 = edge.Q2; Q4 = edge.Q1}))) )))
        )
        
        //((fst inSigma) , (Set.union (Set.empty.Add({Arr1 = (getArr edge.Action); Union2 = Record2 {Q3 = edge.Q2 ; Q4 = edge.Q1} })) (snd inSigma)))