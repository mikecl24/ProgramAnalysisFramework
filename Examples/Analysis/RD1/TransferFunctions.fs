[<AutoOpen>]
module TransferFunctions
open System.Runtime.InteropServices

//Helper code
let rec genIotaV (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genIotaV (next, (Set.union oldIota (Set.empty.Add({Var1 = var; Union1 = List1(QM); Q2 = Node(0)}))))

let extractA a =
    match a with
    | Arr a -> a
    | _     -> failwith "Error: Can only extract array types"

let rec genIotaA (arrs, oldIota) =
    match arrs with
    | [] -> oldIota
    | arr::next -> genIotaA (next, (Set.union oldIota (Set.empty.Add({Var1 = Var (extractA arr); Union1 = List1(QM); Q2 = Node(0)}))))

(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : sigma = Set.union (genIotaV (Variables, Set.empty)) (genIotaA (Arrays, Set.empty))
//printfn "Iota:\n%A\n" (Seq.toList iota)
// Init
let init : sigma = Set.empty



//Helper code
let rec remove (inSet, outSet, killCond) = 
    match inSet with
    | [] -> outSet
    | x::xs when x.Var1=killCond -> remove (xs, outSet, killCond)
    | x::xs -> remove (xs, (Set.union outSet (Set.empty.Add(x))), killCond)

let getVar ast = 
    match ast with
    |   AssignCommand(Var a, b)         -> a
    |   ArrAssignCommand(Arr a, b, c)   -> a
    |   _                           -> failwith "Cannot extract variable from non-assignment"

(*            TRANSFER FUNCTIONS            *)
let TF_Boolean (inSigma : sigma, edge : Edge) : sigma= inSigma

let TF_Assignment (inSigma : sigma, edge : Edge) : sigma = (Set.union (Set.empty.Add({Var1 = Var (getVar edge.Action); Union1 = Q1(edge.Q2);Q2 = edge.Q1})) (remove ((Set.toList inSigma), Set.empty, (Var (getVar edge.Action)))))

let TF_Skip (inSigma : sigma, edge : Edge) : sigma = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : sigma, edge : Edge) : sigma = (Set.union (Set.empty.Add({Var1 = Var (getVar edge.Action); Union1 = Q1(edge.Q2) ;Q2 = edge.Q1})) inSigma)