module Types

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
    {Q1 : int;
    Q2 : int;
    Action : string;
    Type : TFtypes}