module Types

type Node = Node of int
type Var = Var of string

type AnalysisDirection =
    | Forward
    | Backward

type AnalysisOp = 
    | LUB
    | GLB

type StatementType =
    | S_VarAssignment
    | S_Skip
    | S_IfBool
    | S_IfElse
    | S_IfFi
    | S_DoBool
    | S_DoOd
    | S_ArrAssignment

type Statement =
    { text : string;
    s_type : StatementType}

type TFtypes = 
    | Boolean
    | Assignment
    | Skip
    | ArrayAssignment

    
type Edge = 
    {Q1 : Node;
    Q2 : Node;
    Action : string;
    Type : TFtypes}