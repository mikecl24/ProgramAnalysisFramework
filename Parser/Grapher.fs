module Grapher

// Reorders the list of edges by creation order
let rec reverse inList acc=
    match inList with
    | [] -> acc
    | [x] -> x::acc
    | head::tail -> reverse tail (head::acc)

// Corrects the Edge nodes: Replace a node with another (merge)
let rec correctEdges listEd before after =
    match listEd with
    | [] -> []
    | x::xs when x.Q1=before -> [{Q1 = after; Q2 = x.Q2; Action = x.Action; Type = x.Type}] @ (correctEdges xs before after)
    | x::xs when x.Q2=before -> [{Q1 = x.Q1; Q2 = after; Action = x.Action; Type = x.Type}] @ (correctEdges xs before after)
    | _ -> [listEd.Head] @ (correctEdges listEd.Tail before after)


let rec graphIt startNode endNode actList freshNext edgeList ifStart ifEnd doNodes = 
    match actList with
    | [] -> edgeList
    
    | x::xs when x.s_type=S_VarAssignment   
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST; Type = Assignment}]@edgeList) ifStart ifEnd doNodes)
    
    | x::xs when x.s_type=S_Skip           
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST; Type = Skip}]@edgeList) ifStart ifEnd doNodes)
    
    | x::xs when x.s_type=S_IfBool          
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST; Type = Boolean}]@edgeList) (ifStart@[startNode]) ifEnd doNodes)
    
    | x::xs when x.s_type=S_IfElse          
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(ifStart.Head); Q2 = Node(endNode); Action = x.commandAST; Type = Boolean}]@edgeList) (ifStart.Tail) ([startNode]@ifEnd) doNodes)
    
    | x::xs when x.s_type=S_IfFi            
        -> (graphIt ifEnd.Head  startNode   xs freshNext        (correctEdges edgeList edgeList.Head.Q2 (Node(ifEnd.Head))) (ifStart@[startNode]) ifEnd.Tail doNodes)
    
    | x::xs when x.s_type=S_DoBool          
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST; Type = Boolean}]@edgeList) ifStart ifEnd ([startNode]@doNodes))
    
    | x::xs when x.s_type=S_DoOd            
        -> (graphIt startNode   endNode     xs freshNext        ([{Q1 = Node(doNodes.Head); Q2 = Node(startNode); Action = x.commandAST; Type = Boolean}]@correctEdges edgeList edgeList.Head.Q2 (Node(doNodes.Head))) ifStart ifEnd doNodes.Tail)
    
    | x::xs when x.s_type=S_ArrAssignment   
        -> (graphIt endNode     freshNext   xs (freshNext+1)    ([{Q1 = Node(startNode); Q2 = Node(endNode); Action = x.commandAST; Type = ArrayAssignment}]@edgeList) ifStart ifEnd doNodes)
    
    | _ -> failwith "Unknown action type"


let GraphStatements stmtL =  (reverse (graphIt 0 1 stmtL 2 [] [] [] []) [])

let rec extractNodes edgeList nodeSet = 
    match edgeList with
    |  []   -> reverse (Set.fold (fun l se -> se::l) [] nodeSet) []
    |  a::b -> extractNodes b (Set.union nodeSet (Set.empty.Add(a.Q1).Add(a.Q2)))

let ExtractNodes edgeList = extractNodes edgeList Set.empty
     