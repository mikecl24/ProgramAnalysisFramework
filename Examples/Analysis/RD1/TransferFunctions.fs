[<AutoOpen>]
module TransferFunctions

// Helper code
let rec genIotaV (vars, oldIota) =
    match vars with
    | [] -> oldIota
    | var::next -> genIotaV (next, (Set.union oldIota (Set.empty.Add({Ident1 = Var1 var; Union1 = List1(QM); Node2 = Node(0)}))))


let rec genIotaA (arrs, oldIota) =
    match arrs with
    | [] -> oldIota
    | arr::next -> genIotaA (next, (Set.union oldIota (Set.empty.Add({Ident1 = Arr1 arr; Union1 = List1(QM); Node2 = Node(0)}))))

(*            Analysis Type            *)
// Direction
let direction : AnalysisDirection = Forward
// Combination operator
let operation : AnalysisOp = LUB
// Iota
let iota : sigma = Set.union (genIotaV (Variables, Set.empty)) (genIotaA (Arrays, Set.empty))


// Helper code
let rec remove (inSet, outSet, killCond) = 
    match inSet with
    | [] -> outSet
    | x::xs when x.Ident1=killCond -> remove (xs, outSet, killCond)
    | x::xs -> remove (xs, (Set.union outSet (Set.empty.Add(x))), killCond)

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

let TF_Assignment (inSigma : sigma, edge : Edge) : sigma = (Set.union (Set.empty.Add({Ident1 = Var1 (getVar edge.Action); Union1 = Node1(edge.Q2); Node2 = edge.Q1})) (remove ((Set.toList inSigma), Set.empty, (Var1 (getVar edge.Action)))))

let TF_Skip (inSigma : sigma, edge : Edge) : sigma = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma : sigma, edge : Edge) : sigma = (Set.union (Set.empty.Add({Ident1 = Arr1 (getArr edge.Action); Union1 = Node1(edge.Q2) ;Node2 = edge.Q1})) inSigma)