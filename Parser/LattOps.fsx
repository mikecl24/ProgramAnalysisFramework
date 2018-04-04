#load "Domain2.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types

let subset_pw (dom1, dom2) = Set.isSubset dom1 dom2

let subset_m subset_n (dom1, (dom2:Map<'a, 'b>)) = 
    try Map.forall (fun key value -> (subset_n (value, dom2.Item(key)))) dom1
    with e -> false

let rec subset_p (subset_n1, subset_n2) (dom1, dom2) =
    if not (subset_n1 ((fst dom1), (fst dom2)) ) then
        false
    elif not (subset_n2 ((snd dom1), (snd dom2))) then
        false
    else
        true

let x = (Set.empty.Add(1).Add(2), Map.empty.Add(1, Set.empty.Add("x")).Add(2, Set.empty.Add("y")))
let y = (Set.empty.Add(1).Add(2).Add(3), Map.empty.Add(1, Set.empty.Add("x")).Add(2, Set.empty.Add("y")))
printfn "%A" (subset_p (subset_pw, subset_m subset_pw) (x, y) )
printfn "%A" (subset_m subset_pw (snd x,snd y) )
(*
// powerset test
let p1 =   Set.empty.Add( { Var1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) 
let p2 =  Set.empty 
printfn "%A" (subset_pw (p2, p1))

// map test 1
let m1 =  Map.empty.Add( Node(1),  Set.empty.Add( { Var1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) )
let m2 =   Map.empty.Add( Node(1),  Set.empty.Add( { Var1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) )
printfn "%A" ( subset_m subset_pw (m1, m2) )
*)

(*
// map test 2
let m1 =  
                         Map.empty.Add(Node(1), Map.empty.Add( Node(1),  Set.empty.Add( { Var1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) ))
let m2 =  Map.empty.Add(Node(1), Map.empty.Add( Node(1),  Set.empty.Add( { Var1 = Var("y"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) ).Add( Node(1),  Set.empty.Add( { Var1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) ))
printfn "%A" ( subset_m (subset_m subset_pw) (m1, m2) )
*)