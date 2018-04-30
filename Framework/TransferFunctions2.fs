module TransferFunctions
open System.Runtime.InteropServices

//Helper code
let rec genIota (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genIota (next, (Set.union oldIota (Set.empty.Add({Var1 = var; Union1 = List1(QM); Q2 = Node(0)}))))

(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : Powerset1 = genIota (Variables, Set.empty)
//printfn "Iota:\n%A\n" (Seq.toList iota)
// Init
let init : Powerset1 = Set.empty



//Helper code
let rec remove (inSet, outSet, killCond) = 
    match inSet with
    | [] -> outSet
    | x::xs when x.Var1=killCond -> remove (xs, outSet, killCond)
    | x::xs -> remove (xs, (Set.union outSet (Set.empty.Add(x))), killCond)

let getVar ast = 
    match ast with
    |   AssignCommand(a, b)         -> a
    |   ArrAssignCommand(a, b, c)   -> a
    |   _                           -> failwith "Cannot extract variable from non-assignment"

(*            TRANSFER FUNCTIONS            *)
let TF_Boolean (inSigma : Powerset1, edge : Edge) : Powerset1= inSigma

let TF_Assignment (inSigma : Powerset1, edge : Edge) : Powerset1 = (Set.union (Set.empty.Add({Var1 = getVar edge.Action; Union1 = Q1(edge.Q2);Q2 = edge.Q1})) (remove ((Set.toList inSigma), Set.empty, (getVar edge.Action))))

let TF_Skip (inSigma : Powerset1, edge : Edge) : Powerset1 = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : Powerset1, edge : Edge) : Powerset1 = (Set.union (Set.empty.Add({Var1 = getVar edge.Action; Union1 = Q1(edge.Q2) ;Q2 = edge.Q1})) inSigma)
