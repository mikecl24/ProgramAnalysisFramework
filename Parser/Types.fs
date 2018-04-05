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

type aexp = 
    | VarExpr of string
    | NumExpr of int
    | ArrExpr of string * aexp
    | SumExpr of aexp * aexp
    | MinExpr of aexp * aexp
    | MultExpr of aexp * aexp
    | DivExpr of aexp * aexp
    | UMinExpr of aexp

and bexp = 
    | TrueExpr
    | FalseExpr
    | NotExpr of bexp
    | AndExpr of bexp * bexp
    | SAndExpr of bexp * bexp
    | OrExpr of bexp * bexp
    | SOrExpr of bexp * bexp
    | LargerExpr of aexp * aexp
    | LargerEqExpr of aexp * aexp
    | SmallerExpr of aexp * aexp
    | SmallerEqExpr of aexp * aexp
    | EqExpr of aexp * aexp
    | NEqExpr of aexp * aexp

type command = 
    | SkipCommand
    | BoolCommand of bexp
    | AssignCommand of string * aexp
    | ArrAssignCommand of string * aexp * aexp

type Statement =
    { commandAST : command;
    s_type : StatementType}

type TFtypes = 
    | Boolean
    | Assignment
    | Skip
    | ArrayAssignment

type Edge = 
    {Q1 : Node;
    Q2 : Node;
    Action : command;
    Type : TFtypes}
