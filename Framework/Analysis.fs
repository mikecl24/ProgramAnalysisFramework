[<AutoOpen>]
module Analysis
open System.Runtime.CompilerServices


let getInit op = 
    match op with
    | LUB   -> bot
    | GLB   -> top


// Init

let init : sigma = getInit operation  //Set.empty
// printfn "Init (bot/top): %A" init
let reverseEdge (e : Edge) : Edge = 
    {Q1 = e.Q2;
    Q2 = e.Q1;
    Action = e.Action}

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

// Initialization for single node init
let rec initializeSigma (nList : Node list, prevMap : AnalysisResult, node : Node) : AnalysisResult =
    match nList with
    | []    ->  prevMap
    | x::xs ->  if x = node then
                    initializeSigma (xs, (updateAnalysisResult (prevMap, (Map.empty.Add(x, iota)))), node)
                else
                    initializeSigma (xs, (updateAnalysisResult (prevMap, (Map.empty.Add(x, init)))), node)

// The patern matching is the only part dependant on the language!
let rec analysis (worklist : Edge list, edges : Edge list, sigmaMap : AnalysisResult, compOP, combOP) : AnalysisResult = 
    match worklist with
    | []        ->  sigmaMap
    | e1::es    ->  match e1.Action with
                    | BoolCommand(b)                ->  if (compOP ((TF_Boolean (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                            analysis (es, edges, sigmaMap, compOP, combOP)
                                                        else
                                                        analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_Boolean (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)
                    | AssignCommand(v, a)           ->  if (compOP ((TF_Assignment (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                            analysis (es, edges, sigmaMap, compOP, combOP)
                                                        else
                                                            analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_Assignment (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)
                    | SkipCommand                   ->  if (compOP ((TF_Skip (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                            analysis (es, edges, sigmaMap, compOP, combOP)
                                                        else
                                                            analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_Skip (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)
                    | ArrAssignCommand(a, a1, a2)   ->  if (compOP ((TF_ArrayAssignment (sigmaMap.[e1.Q1], e1)), sigmaMap.[e1.Q2])) then 
                                                            analysis (es, edges, sigmaMap, compOP, combOP)
                                                        else
                                                            analysis (edges, edges, (sigmaMap.Add(e1.Q2 ,combOP (((TF_ArrayAssignment (sigmaMap.[e1.Q1], e1))), sigmaMap.[e1.Q2]))), compOP, combOP)

let AnalyseEdges (edgesList: Edge list) : AnalysisResult = 
    let start = initializeSigma (Nodes, (Map.empty), (getFirstNode direction))
    let edges = getAnalysisEdges (edgesList, direction)
    in analysis (edges, edges, start, (getComparator operation), (getCombinator operation))
