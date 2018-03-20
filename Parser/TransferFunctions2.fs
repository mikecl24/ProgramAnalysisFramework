module TransferFunctions


let rec genIota vars oldIota =
    match vars with
    | [] -> oldIota
    | var::next -> genIota next (Set.union oldIota (Set.empty.Add({VAR1 = Var(var); Union1 = List1(QM); Q2 = Node(0)})))

let direction = Forward
let operation = LUB
let iota = genIota Variables Set.empty
let init = Set.empty
printfn "Iota:\n%A\n" (Seq.toList iota)


//Generated code: TF
let TF_Boolean inSigma edge= inSigma

let rec remove (inSet, outSet, killCond) = 
    match inSet with
    | [] -> outSet
    | x::xs when x.VAR1=killCond -> remove (xs, outSet, killCond)
    | x::xs -> remove (xs, (Set.union outSet (Set.empty.Add(x))), killCond)

let TF_Assignment (inSigma:Record1 Set) (edge:Edge) = (Set.union (Set.empty.Add({VAR1 = Var( (edge.Action.Split ':').[0] ); Union1 = Q1(Node(edge.Q2));Q2 = Node(edge.Q1)})) (remove ((Set.toList inSigma), Set.empty, Var((edge.Action.Split ':').[0]))))

let TF_Skip inSigma edge = inSigma

// May not kill in arrays
let TF_ArrayAssignment (inSigma:Record1 Set) (edge:Edge) = (Set.union (Set.empty.Add({VAR1 = Var((edge.Action.Split '[').[0]); Union1 = Q1(Node(edge.Q2)) ;Q2 = Node(edge.Q1)})) inSigma)

