[<AutoOpen>]
module CallGenerator

let fst4 (a, b, c, d) = a
let snd4 (a, b, c, d) = b
let trd4 (a, b, c, d) = c
let frt4 (a, b, c, d) = d

let format (sub, sup, uni, inter) = 
    "[<AutoOpen>]\nmodule Operations\n" +
    "let subsetOP x = " + sub + " x \n" +
    "let supersetOP x = " + sup + " x \n" +
    "let unionOP x = " + uni + " x \n" +
    "let intersectOP x = " + inter + " x \n"
let rec getCalls dom = 
    match dom with
    | PowersetDom(fSet)                 ->  ("subset_pw", "superset_pw", "union_pw", "intersect_pw")
    | TotalFunctionSpaceDom(fSet, fDom) ->  let x = getCalls fDom in
                                            (
                                            "subset_m (" + fst4 x + ")", 
                                            "superset_m (" + snd4 x + ")", 
                                            "union_m (" + trd4 x + ")", 
                                            "intersect_m (" + frt4 x + ")"
                                            )
    | CartesianDom (_)                  -> failwith "Error: Attempt to generate calls in a non-consolidated domain (Cartesian Dom)"
    | CartesianListDom(fListDom)        ->  match fListDom.Length with
                                            | 2 ->  let x1 = getCalls fListDom.[0] 
                                                    let x2 = getCalls fListDom.[1] in
                                                    (
                                                    "subset_p2 (" + fst4 x1 + ", " + fst4 x2 + ")",
                                                    "superset_p2 (" + snd4 x1 + ", " + snd4 x2 + ")",
                                                    "union_p2 (" + trd4 x1 + ", " + trd4 x2 + ")",
                                                    "intersect_p2 (" + frt4 x1 + ", " + frt4 x2 + ")"
                                                    )
                                            | 3 ->  let x1 = getCalls fListDom.[0] 
                                                    let x2 = getCalls fListDom.[1]
                                                    let x3 = getCalls fListDom.[2] in
                                                    (
                                                    "subset_p3 (" + fst4 x1 + ", " + fst4 x2 + ", " + fst4 x3 + ")",
                                                    "superset_p3 (" + snd4 x1 + ", " + snd4 x2 + ", " + snd4 x3 + ")",
                                                    "union_p3 (" + trd4 x1 + ", " + trd4 x2 + ", " + trd4 x3 + ")",
                                                    "intersect_p3 (" + frt4 x1 + ", " + frt4 x2 + ", " + frt4 x3 + ")"
                                                    )
                                            | _ -> failwith "Error: Cartesian domains with maximum 3 terms (for now)"
    | SetDom(_)                         -> ("subset_s", "superset_s", "union_s", "intersect_s")
let evaluateASTCalls dom = format (getCalls dom)

