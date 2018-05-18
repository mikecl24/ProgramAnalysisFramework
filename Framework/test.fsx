
#load "Types.fs"
#load "LattOps.fs"
#load "Domain.fs"

let evalB (b, s) =
    match b with 
    | TrueExpr         -> 
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


let sigma1 = Map.empty.Add(Var1 (Var "x"), Set.empty.Add(Zero).Add(Minus)).Add(Var1 (Var "y"), Set.empty.Add(Plus))


printfn "%A" (evalA (UMinExpr ( VarExpr(Var "x") ), sigma1))