module Analysis


let rec initializeSigma nList = 
    match nList with
    | [] -> printfn ""
    | x::xs -> AnalysisResult.Add(x, init);
               initializeSigma xs

               
initializeSigma Nodes
AnalysisResult.Remove 0
AnalysisResult.Add(0, iota)
printfn "Initial analysis state:\n%A\n" (Seq.toList AnalysisResult)

let rec genWorklist lEdges oldSet = 
    match lEdges with
    | [] -> oldSet
    | x::xs -> ( genWorklist xs (Set.union oldSet (Set.empty.Add(x))) )

//let worklist = genWorklist [0..Edges.Length-1] Set.empty
//printfn "Worklist:\n%A\n" worklist

let mutable oldSet = Set.empty

let rec Analysis edgeList Edges = 
    match edgeList with
    | [] -> AnalysisResult
    | edge1::edgeL -> 
        match edge1.Type with
        | Boolean -> if Set.isSubset (TF_Boolean AnalysisResult.[edge1.Q1] edge1) AnalysisResult.[edge1.Q2] then 
                        Analysis edgeL Edges
                     else
                        oldSet <- AnalysisResult.[edge1.Q2]
                        AnalysisResult.Remove edge1.Q2
                        AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_Boolean AnalysisResult.[edge1.Q1] edge1)))
                        Analysis Edges Edges
        
        | Assignment -> if Set.isSubset (TF_Assignment (AnalysisResult.Item(edge1.Q1)) edge1) AnalysisResult.[edge1.Q2] then  
                            Analysis edgeL Edges
                        else
                            oldSet <- AnalysisResult.[edge1.Q2]
                            AnalysisResult.Remove edge1.Q2
                            AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_Assignment AnalysisResult.[edge1.Q1] edge1)))
                            Analysis Edges Edges
                            
        | Skip -> if Set.isSubset (TF_Skip AnalysisResult.[edge1.Q1] edge1) AnalysisResult.[edge1.Q2] then
                    Analysis edgeL Edges
                  else
                    oldSet <- AnalysisResult.[edge1.Q2]
                    AnalysisResult.Remove edge1.Q2
                    AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_Skip AnalysisResult.[edge1.Q1] edge1)))
                    Analysis Edges Edges
        
        | ArrayAssignment -> if Set.isSubset (TF_ArrayAssignment AnalysisResult.[edge1.Q1] edge1) AnalysisResult.[edge1.Q2] then
                                 Analysis edgeL Edges
                             else
                                 oldSet <- AnalysisResult.[edge1.Q2]
                                 AnalysisResult.Remove edge1.Q2
                                 AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_ArrayAssignment AnalysisResult.[edge1.Q1] edge1)))
                                 Analysis Edges Edges
 
let AnalyseEdges Edges = Analysis Edges Edges
