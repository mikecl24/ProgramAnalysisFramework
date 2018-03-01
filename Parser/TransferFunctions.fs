module TransferFunctions

//Generated by TF
let rec genIota vars oldIota =
    match vars with
    | [] -> oldIota
    | var::next -> genIota next (Set.union oldIota (Set.empty.Add({Var = var; Q1 = Val(E_QM_); Q2 = 0})))
    
let iota = genIota Variables Set.empty
let init = Set.empty
printfn "Iota:\n%A\n" (Seq.toList iota)


//Generated code: TF
let TF_Boolean inSigma edge= inSigma

let rec remove inSet outSet killCond = 
    match inSet with
    | [] -> outSet
    | x::xs when x.Var=killCond -> remove xs outSet killCond
    | x::xs -> remove xs (Set.union outSet (Set.empty.Add(x))) killCond

let TF_Assignment (inSigma:sigma Set) (edge:Edge) = (Set.union (Set.empty.Add({Var = (edge.Action.Split ':').[0]; Q1 = Q(edge.Q1) ;Q2 = edge.Q2})) (remove (Set.toList inSigma) Set.empty (edge.Action.Split ':').[0]))

let TF_Skip inSigma edge = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma:sigma Set) (edge:Edge) = (Set.union (Set.empty.Add({Var = (edge.Action.Split '[').[0]; Q1 = Q(edge.Q1) ;Q2 = edge.Q2})) inSigma)

