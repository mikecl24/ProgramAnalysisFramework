#load "Domain2.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types

let subset_pw (dom1, dom2) = Set.isSubset dom1 dom2

let subset_m subset_n ((dom1:Map<'a, 'b>), (dom2:Map<'a, 'b>)) = 
    if dom1.Count = dom2.Count then
        try Map.forall (fun key value -> (subset_n (value, dom2.Item(key)))) dom1
        with e -> false
    else
        false
let rec subset_p (subset_n1, subset_n2) (dom1, dom2) =
    if not (subset_n1 ((fst dom1), (fst dom2)) ) then
        false
    elif not (subset_n2 ((snd dom1), (snd dom2))) then
        false
    else
        true

let superset_pw (dom1, dom2) =  Set.isSuperset dom1 dom2

let superset_m superset_n ((dom1:Map<'a, 'b>), (dom2:Map<'a, 'b>)) = 
    if dom1.Count = dom2.Count then
        try Map.forall (fun key value -> (superset_n (value, dom2.Item(key)))) dom1
        with e -> false
    else
        false

let rec superset_p (superset_n1, superset_n2) (dom1, dom2) =
    if not (superset_n1 ((fst dom1), (fst dom2)) ) then
        false
    elif not (superset_n2 ((snd dom1), (snd dom2))) then
        false
    else
        true
(*
let x = Map.empty.Add(1, Set.empty.Add(1).Add(2).Add(3)).Add(3, Set.empty.Add(1).Add(2).Add(3))
let y = Map.empty.Add(1, Set.empty.Add(1).Add(2)).Add(3, Set.empty)
printfn "%A" (superset_m superset_pw (x,y))
*)

let x = (Set.empty.Add(1).Add(2), 
            Map.empty.Add("text1", Map.empty.Add(1, Set.empty.Add("x")).Add(2, Set.empty.Add("y").Add("z")))
        )
let y = (Set.empty.Add(1), 
            Map.empty.Add("text1", Map.empty.Add(1, Set.empty.Add("x")).Add(2, Set.empty.Add("y")))
        )
printfn "%A" (superset_p (superset_pw, (superset_m (superset_m superset_pw))) (x, y) )
