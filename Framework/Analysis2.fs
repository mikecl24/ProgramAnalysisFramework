module Analysis

let getValueAR (AnalysisResult v) = v

let updateAnalysisResult ((p:AnalysisResult), (q:AnalysisResult)) = 
    AnalysisResult(Map(Seq.concat [ (Map.toSeq (getValueAR p)) ; (Map.toSeq (getValueAR q)) ]))

let getFirstNode dir =
    match dir with
    | Forward   ->  0
    | Backward  ->  printfn "Make sure last node is always the final node!"
                    Nodes.[Nodes.Length-1]

let rec initializeSigma (nList, prevMap, node) = 
    match nList with
    | []    ->  prevMap
    | x::xs ->  if x = node then
                    initializeSigma (xs, (updateAnalysisResult (prevMap, AnalysisResult(Map.empty.Add(Node(x), iota)))), node)
                else
                    initializeSigma (xs, (updateAnalysisResult (prevMap, AnalysisResult(Map.empty.Add(Node(x), init)))), node)

let fNode = getFirstNode direction
let initRes = initializeSigma (Nodes, AnalysisResult(Map.empty), fNode)
printfn "Initial analysis state:\n%A\n" (Seq.toList (getValueAR initRes))

let rec genWorklist lEdges oldSet = 
    match lEdges with
    | [] -> oldSet
    | x::xs -> ( genWorklist xs (Set.union oldSet (Set.empty.Add(x))) )



let mutable oldSet = Set.empty

let rec analysis (edgeList edges, result) = 
    match edgeList with
    | [] -> result
    | edge1::edgeL -> 
        match edge1.Type with
        | Boolean -> if Set.isSubset (TF_Boolean AnalysisResult.[edge1.Q1] edge1) AnalysisResult.[edge1.Q2] then 
                        Analysis edgeL edges
                     else
                        oldSet <- AnalysisResult.[edge1.Q2]
                        AnalysisResult.Remove edge1.Q2
                        AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_Boolean AnalysisResult.[edge1.Q1] edge1)))
                        Analysis edges edges
        
        | Assignment -> if Set.isSubset (TF_Assignment (AnalysisResult.Item(edge1.Q1)) edge1) AnalysisResult.[edge1.Q2] then  
                            Analysis edgeL edges
                        else
                            oldSet <- AnalysisResult.[edge1.Q2]
                            AnalysisResult.Remove edge1.Q2
                            AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_Assignment AnalysisResult.[edge1.Q1] edge1)))
                            Analysis edges edges
                            
        | Skip -> if Set.isSubset (TF_Skip AnalysisResult.[edge1.Q1] edge1) AnalysisResult.[edge1.Q2] then
                    Analysis edgeL edges
                  else
                    oldSet <- AnalysisResult.[edge1.Q2]
                    AnalysisResult.Remove edge1.Q2
                    AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_Skip AnalysisResult.[edge1.Q1] edge1)))
                    Analysis edges edges
        
        | ArrayAssignment -> if Set.isSubset (TF_ArrayAssignment AnalysisResult.[edge1.Q1] edge1) AnalysisResult.[edge1.Q2] then
                                 Analysis edgeL edges
                             else
                                 oldSet <- AnalysisResult.[edge1.Q2]
                                 AnalysisResult.Remove edge1.Q2
                                 AnalysisResult.Add(edge1.Q2, (Set.union oldSet (TF_ArrayAssignment AnalysisResult.[edge1.Q1] edge1)))
                                 Analysis edges edges
 
let AnalyseEdges Edges = analysis (Edges, Edges, AnalysisResult(Map.empty))
