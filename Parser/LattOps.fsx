#load "Domain2.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types


let subset_m subset_n (dom1, (dom2:Map<'a, 'b>)) = 
    match (dom1, dom2) with
    | (x, y) -> try Map.forall (fun key value -> (subset_n (value, y.Item(key)))) x
                                                with e -> false
    | _ -> failwith "Unimplemented other map type!"




//let subset_m subset_n (dom1:Map<'a,'b>, dom2) = Map.forall (fun key value -> (subset_n (value, dom2.[key]))) dom1

let subset_p subset_n (set1, set2) = "Unimplemented subset in domains with products!"



// powerset test
let p1 =  Set.empty.Add( { VAR1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) 
let p2 =  Set.empty 
printfn "%A" (subset_pw (p2, p1))

// map test
let m1 =  
                         Map.empty.Add( Node(1),  Set.empty.Add( { VAR1 = Var("x"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) )
                        
printfn "%A" m1
let m2 =   Map.empty.Add( Node(1),  Set.empty.Add( { VAR1 = Var("y"); 
                                     Union1 = Q1( Node(1) );
                                     Q2 = Node(2) } ) )
                       

printfn "%A" ( subset_m subset_pw (m1, m2) )