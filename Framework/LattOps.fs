[<AutoOpen>]
module LatticeOperations

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

let union_pw (dom1, dom2) = Set.union dom1 dom2

let union_m union_n ((dom1:Map<'a, 'b>), (dom2:Map<'a, 'b>)) = 
    if dom1.Count = dom2.Count then
        try Map.map (fun key value -> (union_n (value, dom2.Item(key)))) dom1
        with e -> failwith "Union of maps with different keys!"
    else
        failwith "Union of maps with different keys!"

let rec union_p (union_n1, union_n2) (dom1, dom2) =
    (union_n1 ((fst dom1), (fst dom2)),union_n2 ((snd dom1), (snd dom2)))

let intersect_pw (dom1, dom2) = Set.intersect dom1 dom2

let intersect_m intersect_n ((dom1:Map<'a, 'b>), (dom2:Map<'a, 'b>)) = 
    if dom1.Count = dom2.Count then
        try Map.map (fun key value -> (intersect_n (value, dom2.Item(key)))) dom1
        with e -> failwith "Intersection of maps with different keys!"
    else
        failwith "Intersection of maps with different keys!"

let rec intersect_p (intersect_n1, intersect_n2) (dom1, dom2) =
    (intersect_n1 ((fst dom1), (fst dom2)),intersect_n2 ((snd dom1), (snd dom2)))
