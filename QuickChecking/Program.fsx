#r "FsCheck.dll"

#load "Domain.fs"
open Domain

open FsCheck
open System
open System.Threading
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic
open Prop

(*      SAMPLE SETS     *)

let rec genVar num: Var list = 
    match num with
    | 0 -> [Var "var0"]
    | x -> (genVar (x-1)) @ [Var ("var" + string(x))]

let rec genNode num: Node list = 
    match num with
    | 0 -> [Node num]
    | x -> (genNode (x-1)) @ [Node x]

let genSets (sn, sv) (mn, mv) (ln, lv) (pn, pv) = 
    let r_sn = genNode sn
    let r_mn = genNode mn
    let r_ln = genNode ln
    let r_pn = genNode pn
    let r_sv = genVar sv
    let r_mv = genVar mv
    let r_lv = genVar lv
    let r_pv = genVar pv
    (r_sn, r_mn, r_ln, r_pn, r_sv, r_mv, r_lv, r_pv)


(*      USAGE            *)
let (nodeS, nodeM, nodeL, nodeP, varS, varM, varL, varP) = (genSets (4,8) (10, 20) (20, 100) (10, 10))



(*      GENERATORS       *)

type CustomGeneratorSmall =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements nodeS //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements varS //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }

Arb.register<CustomGeneratorSmall>()

type CustomGeneratorMedium =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements nodeM //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements varM //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }

Arb.register<CustomGeneratorMedium>()

type CustomGeneratorLarge =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements nodeL //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements varL //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }

Arb.register<CustomGeneratorLarge>()

type CustomGeneratorProgram =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements nodeP //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements varP //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }

Arb.register<CustomGeneratorProgram>()


    

///////////////////////////////////////////////
let tf_map (x:AnalysisResult) = x

let subset_pw (dom1, dom2) = Set.isSubset dom1 dom2

let subset_m subset_n ((dom1:Map<'a, 'b>), (dom2:Map<'a, 'b>)) = 
    if dom1.Count = dom2.Count then
        try Map.forall (fun key value -> (subset_n (value, dom2.Item(key)))) dom1
        with e -> false
    else
        false
let rec subset_p (subset_n1, subset_n2) (dom1, dom2) =
    if not (subset_n1 ((fst dom1), (fst dom2)) ) then
        false
    elif not (subset_n2 ((snd dom1), (snd dom2))) then
        false
    else
        true

let checkMaps (x:AnalysisResult, y:AnalysisResult) = (subset_m (subset_pw) (x, y)) ==> (lazy (subset_m (subset_pw) ((tf_map x), (tf_map y))))
Check.Quick checkMaps

///////////////////////////////////////////////////////////////////

// let tf_test (x:Powerset1):Powerset1 = 
//         if (x = Set.empty) then
//             Set.empty.Add( {Var1 = Var "Var1";
//             Union1 = Q1 (Node 1);
//             Q2 = Node 2;} )
//         else    
//             Set.empty

let tf_test x = Set.empty

let checkMonotonicity (x:Powerset1, y:Powerset1) = (Set.isSubset x y) ==> (lazy (Set.isSubset (tf_test x) (tf_test y)))
printfn "Checking Monotonicity for Program sized programs in TF_test"
Check.Quick checkMonotonicity


let check (x, y) = (x>y) ==>  (x>y-1)
Check.QuickThrowOnFailure check