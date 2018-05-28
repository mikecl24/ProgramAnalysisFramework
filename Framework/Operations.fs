[<AutoOpen>]
module Operations
let subsetOP x = subset_p2 (subset_pw, subset_pw) x 
let supersetOP x = superset_p2 (superset_pw, superset_pw) x 
let unionOP x = union_p2 (union_pw, union_pw) x 
let intersectOP x = intersect_p2 (intersect_pw, intersect_pw) x 
