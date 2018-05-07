[<AutoOpen>]
module Analysis
open System.Runtime.CompilerServices

let reverseEdge (e : Edge) : Edge = 
    {Q1 = e.Q2;
    Q2 = e.Q1;
    Action = e.Action;
    Type = e.Type}

let rec reverseEdges (old : Edge list) : Edge list = 
    match old with
    | []    ->  []
    | a::b  ->  [reverseEdge a] @ reverseEdges b

let getAnalysisEdges (edgesList : Edge list, direction : AnalysisDirection) : Edge list = 
    match direction with
    | Forward   ->  edgesList
    | Backward  ->  reverseEdges edgesList

let getFirstNode (dir : AnalysisDirection) : Node =
    match dir with
    | Forward   ->  Node 0
    | Backward  ->  Edges.[Edges.Length-1].Q2   //Grapher last edge will always connect to last node (happening naturally or by merge)

let getComparator (op : AnalysisOp) =
    match op with
    | LUB   ->  subsetOP
    | GLB   ->  supersetOP

let getCombinator (op : AnalysisOp) =
    match op with
    | LUB   ->  unionOP
    | GLB   ->  intersectOP

let updateAnalysisResult (p : AnalysisResult, q : AnalysisResult) : AnalysisResult = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let rec initializeSigma (nList : Node list, prevMap : AnalysisResult, node : Node) : AnalysisResult =
    match nList with
    | []    ->  prevMap
    | x::xs ->  if x = node then
                    initializeSigma (xs, (updateAnalysisResult (prevMap, (Map.empty.Add(x, iota)))), node)
                else
                    initializeSigma (xs, (updateAnalysisResult (prevMap, (Map.empty.Add(x, init)))), node)
   

let rec analysis (worklist : Edge list, edges : Edge list, sigmaMap : AnalysisResult, compOP, combOP) : AnalysisResult = 
    match worklist with
    | []        ->  sigmaMap
    | e1::es    ->  match e1.Type with
                    | Boolean           ->  if (compOP ((TF_Boolean (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                analysis (es, edges, sigmaMap, compOP, combOP)
                                            else
                                                analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_Boolean (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)
                    | Assignment        ->  if (compOP ((TF_Assignment (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                analysis (es, edges, sigmaMap, compOP, combOP)
                                            else
                                                analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_Assignment (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)
                    | Skip              ->  if (compOP ((TF_Skip (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                analysis (es, edges, sigmaMap, compOP, combOP)
                                            else
                                                analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_Skip (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)
                    | ArrayAssignment   ->  if (compOP ((TF_ArrayAssignment (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                analysis (es, edges, sigmaMap, compOP, combOP)
                                            else
                                                analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_ArrayAssignment (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)

let AnalyseEdges (edgesList: Edge list) : AnalysisResult = 
    let start = initializeSigma (Nodes, (Map.empty), (getFirstNode direction))
    let edges = getAnalysisEdges (edgesList, direction)
    in analysis (edges, edges, start, (getComparator operation), (getCombinator operation))
