
#load "Domain.fs"


let x : AnalysisResult = Map.empty.Add(Node 1, 
                                            Map.empty.Add(Var1 (Var "x"), Record1 {Union3 = List2 NegInf; Union4 = List3 PlusInf}) )
                                  .Add(Node 2, 
                                            Map.empty)
let y : AnalysisResult = Map.empty.Add(Node 1, 
                                            Map.empty.Add(Var1 (Var "x"), Record1 {Union3 = List2 NegInf; Union4 = List3 PlusInf}) )
                                  .Add(Node 2, 
                                            Map.empty)
//printfn "%A" x
//printfn "%A" (superset_s ({Union2 = List1 NegInf; Union3 = Int2 10}, {Union2 = Int1 1; Union3 = Int2 10}))
printfn "%A" (intersect_s ({Union3 = List2 NegInf; Union4 = List3 PlusInf}, {Union3 = List2 NegInf; Union4 = Int2 11}))
