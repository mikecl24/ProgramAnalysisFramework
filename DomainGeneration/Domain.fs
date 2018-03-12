module Domain

//Generated Code Section: Domain type
// Q -> P( Var * (Q U {QM}) * Q )

// List1 = {QM}
type List1 =
    | L1_QM
    | L1_nofrew

// Union1 = (Q2 U List1)
type Union1 = 
    | Q2 of int
    | U1_List1 of List1 // if it is "?"

// Record1 = Var1 * Union1 * Q2
type Record1 =
    {VAR1 : string;
    R1_Union1 : Union1;
    Q1 : int
    }

// Set1 = P( Record1 )
type Set1 = Record1 Set

//  Q -> Set1
let AnalysisResult = new Dictionary<int, Set1>()