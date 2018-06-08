[<AutoOpen>]
module Grapher


// Corrects the Edge nodes: Replace a node with another (for merging branches)
let rec correctEdges (edges : Edge list) (before : Node) (after : Node) : Edge list =
    match edges with
    | []                        -> []
    | x::xs when x.Q1=before    -> [{Q1 = after; Q2 = x.Q2; Action = x.Action}] @ (correctEdges xs before after)
    | x::xs when x.Q2=before    -> [{Q1 = x.Q1; Q2 = after; Action = x.Action}] @ (correctEdges xs before after)
    | _                         -> [edges.Head] @ (correctEdges edges.Tail before after)

// Create a graph from the list of statements
let rec graphIt (startNode : int) (endNode : int) (actions : Statement list) (freshNext : int) (edges : Edge list) (ifStart : int list) (ifEnd : int list) (doNodes : int list) : Edge list = 
    match actions with
    | [] -> edges
    
    | x::xs when x.s_type=S_VarAssignment   
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST}]@edges) ifStart ifEnd doNodes)
    
    | x::xs when x.s_type=S_Skip           
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST}]@edges) ifStart ifEnd doNodes)
    
    | x::xs when x.s_type=S_IfBool          
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST}]@edges) (ifStart@[startNode]) ifEnd doNodes)
    
    | x::xs when x.s_type=S_IfElse          
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(ifStart.Head); Q2 = Node(endNode); Action = x.commandAST}]@edges) (ifStart.Tail) ([startNode]@ifEnd) doNodes)
    
    | x::xs when x.s_type=S_IfFi            
        -> (graphIt ifEnd.Head  startNode   xs freshNext        (correctEdges edges edges.Head.Q2 (Node(ifEnd.Head))) (ifStart@[startNode]) ifEnd.Tail doNodes)
    
    | x::xs when x.s_type=S_DoBool          
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST}]@edges) ifStart ifEnd ([startNode]@doNodes))
    
    | x::xs when x.s_type=S_DoOd            
        -> (graphIt startNode   endNode     xs freshNext        ([{Q1 = Node(doNodes.Head); Q2 = Node(startNode); Action = x.commandAST}]@correctEdges edges edges.Head.Q2 (Node(doNodes.Head))) ifStart ifEnd doNodes.Tail)
    
    | x::xs when x.s_type=S_ArrAssignment   
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST}]@edges) ifStart ifEnd doNodes)
    
    | _ -> failwith "Unknown action type"

// Call graph creation function with initial parameters
let GraphStatements stmtL =  reverse (graphIt 0 1 stmtL 2 [] [] [] [])

// Get a list of all the nodes used in the graph
let rec extractNodes (edges : Edge list) (nodes : Node Set) : Node list = 
    match edges with
    |  []   -> reverse (Set.fold (fun l se -> se::l) [] nodes)
    |  a::b -> extractNodes b (Set.union nodes (Set.empty.Add(a.Q1).Add(a.Q2)))

// Call node extraction function with initial parameters
let ExtractNodes (edges : Edge list) : Node list = extractNodes edges Set.empty
     