open System 

type statement =
    { text : string;
    typeNr : int}

type TFtypes = 
    | Boolean
    | Assignment
    | Skip
    | ArrayAssignment
    
type edge = 
    {Q1 : int;
    Q2 : int;
    Action : string;
    Type : TFtypes}

let rec rev lis acc=
    match lis with
    | [] -> acc
    | [x] -> x::acc
    | head::tail -> rev tail (head::acc)
    
let rec correctEdges listEd before after =
    match listEd with
    | [] -> []
    | x::xs when x.Q1=before -> [{Q1 = after; Q2 = x.Q2; Action = x.Action; Type = x.Type}] @ (correctEdges xs before after)
    | x::xs when x.Q2=before -> [{Q1 = x.Q1; Q2 = after; Action = x.Action; Type = x.Type}] @ (correctEdges xs before after)
    | _ -> [listEd.Head] @ (correctEdges listEd.Tail before after)


let rec graphIt startNode endNode actList freshNext edgeList ifStart ifEnd doNodes = 
    match actList with
    | [] -> edgeList
    | x::xs when x.typeNr=1 -> (graphIt endNode freshNext xs (freshNext+1) ([{Q1 = startNode; Q2 = endNode; Action = x.text; Type = Assignment}]@edgeList) ifStart ifEnd doNodes)
    | x::xs when x.typeNr=2 -> (graphIt endNode freshNext xs (freshNext+1) ([{Q1 = startNode; Q2 = endNode; Action = x.text; Type = Skip}]@edgeList) ifStart ifEnd doNodes)
    | x::xs when x.typeNr=3 -> (graphIt endNode freshNext xs (freshNext+1) ([{Q1 = startNode; Q2 = endNode; Action = x.text; Type = Boolean}]@edgeList) (ifStart@[startNode]) ifEnd doNodes)
    | x::xs when x.typeNr=4 -> (graphIt endNode freshNext xs (freshNext+1) ([{Q1 = ifStart.Head; Q2 = endNode; Action = x.text; Type = Boolean}]@edgeList) (ifStart.Tail) ([startNode]@ifEnd) doNodes)
    | x::xs when x.typeNr=5 -> (graphIt ifEnd.Head startNode xs freshNext (correctEdges edgeList edgeList.Head.Q2 ifEnd.Head) (ifStart@[startNode]) ifEnd.Tail doNodes)
    | x::xs when x.typeNr=6 -> (graphIt endNode freshNext xs (freshNext+1) ([{Q1 = startNode; Q2 = endNode; Action = x.text; Type = Boolean}]@edgeList) ifStart ifEnd ([startNode]@doNodes))
    | x::xs when x.typeNr=7 -> (graphIt startNode endNode xs freshNext ([{Q1 = doNodes.Head; Q2 = startNode; Action = x.text; Type = Boolean}]@correctEdges edgeList edgeList.Head.Q2 doNodes.Head) ifStart ifEnd doNodes.Tail)
    | x:xs when x.typeNr=8 -> (graphIt endNode freshNext xs (freshNext+1) ([{Q1 = startNode; Q2 = endNode; Action = x.text; Type = ArrayAssignment}]@edgeList) ifStart ifEnd doNodes)
    | _ -> failwith "Unknown action type"


let stmts =  [{text = "true";
  typeNr = 3;}; {text = "skip";
                 typeNr = 2;}; {text = "x := 0";
                                typeNr = 1;}; {text = "!(true)";
                                               typeNr = 4;}; {text = "x := 0";
                                                              typeNr = 1;};
 {text = "y := 1";
  typeNr = 1;}; {text = "FI";
                 typeNr = 5;}; {text = "skip";
                                typeNr = 2;}]

printfn "%A" (rev (graphIt 0 1 stmts 2 [] [] [] []) [] )