module Domain

//Generated Code Section: Domain type
// Q -> P( Var * (Q U {QM}) * Q )

type Node = Node of int
type Var = Var of string

// List1 = {QM}
type List1 =
    | QM

// Union1 = (Q2 U List1)
type Union1 = 
    | Q1 of Node
    | List1 of List1 // if it is "?"

// Record1 = Var1 * Union1 * Q2
type Record1 =
    {VAR1 : Var;
    Union1 : Union1;
    Q2 : Node
    }

// Set1 = P( Record1 )
type Powerset1 = Record1 Set

//  Q -> Set1
type AnalysisResult = AnalysisResult of Map<Node,Powerset1>
//let AnalysisResult = new Dictionary<int, Powerset1>()