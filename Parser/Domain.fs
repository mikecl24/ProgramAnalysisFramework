module Domain

//Generated Code Section: Domain type
// Q -> P( Var * (Q U {?}) * Q )

// elem1 = {?}
type elem1 =
    | E_QM_

// union1 = Q2 U {?}
type union1 = 
    | Q of int
    | List of elem1 // if it is "?"

// sigma = Var1 * union1 * Q1
type sigma =
    {Var : string;
    Q1 : union1;
    Q2 : int
    }

// Domain = P( sigma )
type Sigma = sigma Set

//  Q -> Domain
let AnalysisResult = new Dictionary<int, Sigma>()