[<AutoOpen>]
module QCheckGenerator

open System.IO                      // File Handling

(* Map Builders *)
let rec customS m (mapDescriptor : Map<int, (string*string*string)>) : string = 
    match m with
    |   a when a=1  -> ""
    |   a when a>1  -> "\n    static member Map" + (a-1).ToString() + "() = 
      {new Arbitrary<Map"+ (a-1).ToString() + ">() with
          override x.Generator = createAnalysis " + fst3 (mapDescriptor.[a-1]) + "S.Length " + fst3 (mapDescriptor.[a-1]) +  "S ( Arb.generate<" + trd3 (mapDescriptor.[a-1]) + "> |> Gen.listOfLength " + fst3 (mapDescriptor.[a-1]) + "S.Length |> Gen.sample 1000000 1).[0] \n          override x.Shrinker t = Seq.empty }\n " + (customS (a-1) mapDescriptor)
    |   _  -> failwith "Error: Invalid map amount!"

let rec customM m (mapDescriptor : Map<int, (string*string*string)>) : string = 
    match m with
    |   a when a=1  -> ""
    |   a when a>1  -> "    static member Map" + (a-1).ToString() + "() = 
      {new Arbitrary<Map"+ (a-1).ToString() + ">() with
          override x.Generator = createAnalysis " + fst3 (mapDescriptor.[a-1]) + "M.Length " + fst3 (mapDescriptor.[a-1]) +  "M ( Arb.generate<" + trd3 (mapDescriptor.[a-1]) + "> |> Gen.listOfLength " + fst3 (mapDescriptor.[a-1]) + "M.Length |> Gen.sample 1000000 1).[0] \n          override x.Shrinker t = Seq.empty }\n " + (customM (a-1) mapDescriptor)
    |   _  -> failwith "Error: Invalid map amount!"

let rec customL m (mapDescriptor : Map<int, (string*string*string)>) : string = 
    match m with
    |   a when a=1  -> ""
    |   a when a>1  -> "    static member Map" + (a-1).ToString() + "() = 
      {new Arbitrary<Map"+ (a-1).ToString() + ">() with
          override x.Generator = createAnalysis " + fst3 (mapDescriptor.[a-1]) + "L.Length " + fst3 (mapDescriptor.[a-1]) +  "L ( Arb.generate<" + trd3 (mapDescriptor.[a-1]) + "> |> Gen.listOfLength " + fst3 (mapDescriptor.[a-1]) + "L.Length |> Gen.sample 1000000 1).[0] \n          override x.Shrinker t = Seq.empty }\n " + (customL (a-1) mapDescriptor)
    |   _  -> failwith "Error: Invalid map amount!"



let header par = @"
// Generated QuickChecking executable...
#r ""FsCheck.dll""

// Fake Meta sets so TF compile:
let Identifiers = []
let Variables = []
let Arrays = []
let Nodes = []

#load ""Types.fs""
#load ""Domain.fs""
#load ""TransferFunctions.fs""
#load ""LattOps.fs""
#load ""Operations.fs""

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
    | 0 -> [Var ""var0""]
    | x -> (genVar (x-1)) @ [Var (""var"" + string(x))]

let rec genArr num: Arr list = 
    match num with
    | 0 -> [Arr ""arr0""]
    | x -> (genArr (x-1)) @ [Arr (""arr"" + string(x))]

let rec genNode num: Node list = 
    match num with
    | 0 -> [Node num]
    | x -> (genNode (x-1)) @ [Node x]

let genIdent v a = 
    List.map (fun x -> Var1 x) v @
    List.map (fun x -> Arr1 x) a

let genSets (sn, sv, sa) (mn, mv, ma) (ln, lv, la) = 
    let r_sn = genNode sn
    let r_mn = genNode mn
    let r_ln = genNode ln
    let r_sv = genVar sv
    let r_mv = genVar mv
    let r_lv = genVar lv
    let r_sa = genArr sa
    let r_ma = genArr ma
    let r_la = genArr la
    let r_si = genIdent r_sv r_sa
    let r_mi = genIdent r_mv r_ma
    let r_li = genIdent r_lv r_la
    (r_sn, r_mn, r_ln, r_sv, r_mv, r_lv, r_sa, r_ma, r_la, r_si, r_mi, r_li)


(*            USAGE            *)
let (NodesS, NodesM, NodesL, VariablesS, VariablesM, VariablesL, ArraysS, ArraysM, ArraysL, IdentifiersS, IdentifiersM, IdentifiersL) = (genSets " + par + ")\n\n

(*            Map Generating helpers            *)
let rec createAnalysisH list1 (valueL : 'a list) =
    match list1 with
    | []    -> Map.empty
    | a::b  -> Map.fold (fun acc key value -> Map.add key value acc) (Map.empty.Add(a, valueL.Head)) (createAnalysisH b valueL.Tail) 

let createAnalysis length xs ys= 
  gen { let! i = Gen.elements ys |> Gen.listOfLength length
        return (createAnalysisH xs i) }

(*            TF matching              *)
let TF_Analysis (inSigma : sigma, edge : Edge) : sigma = 
    match edge.Action with
    | SkipCommand           -> TF_Skip (inSigma, edge)
    | BoolCommand(_)        -> TF_Boolean (inSigma, edge)
    | AssignCommand(_)      -> TF_Assignment (inSigma, edge)
    | ArrAssignCommand(_)   -> TF_ArrayAssignment (inSigma, edge)\n

(*      GENERATORS SMALL       *)
type CustomGeneratorSmall =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements NodesS
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements VariablesS 
          override x.Shrinker t = Seq.empty }
     static member Arr() =
      { new Arbitrary<Arr>() with
            override x.Generator = Gen.elements ArraysS 
            override x.Shrinker t = Seq.empty }
"

let botS = @"

Arb.register<CustomGeneratorSmall>()

let checkMonotonicity1s (x:sigma, y:sigma, e:Edge) = (subsetOP (x, y))  ==> (lazy (subsetOP ((TF_Analysis (x, e)), (TF_Analysis (y, e)))))
let checkMonotonicity2s (x:sigma, y:sigma, e:Edge) = (supersetOP (x, y))  ==> (lazy (supersetOP ((TF_Analysis (x, e)), (TF_Analysis (y, e)))))

printfn ""Checking Monotonicity for Small sized programs""
Check.Quick checkMonotonicity1s
Check.Quick checkMonotonicity2s

"
let genM = "(*      GENERATORS MEDIUM       *)
type CustomGeneratorMedium =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements NodesM
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements VariablesM 
          override x.Shrinker t = Seq.empty }
     static member Arr() =
      { new Arbitrary<Arr>() with
            override x.Generator = Gen.elements ArraysM
            override x.Shrinker t = Seq.empty }
"
let botM = @"

Arb.register<CustomGeneratorMedium>()

let checkMonotonicity1m (x:sigma, y:sigma, e:Edge) = (subsetOP (x, y))  ==> (lazy (subsetOP ((TF_Analysis (x, e)), (TF_Analysis (y, e)))))
let checkMonotonicity2m (x:sigma, y:sigma, e:Edge) = (supersetOP (x, y))  ==> (lazy (supersetOP ((TF_Analysis (x, e)), (TF_Analysis (y, e)))))

printfn ""Checking Monotonicity for Medium sized programs""
Check.Quick checkMonotonicity1m
Check.Quick checkMonotonicity2m

"
let genL = "(*      GENERATORS LARGE       *)
type CustomGeneratorLarge =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements NodesL
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements VariablesL
          override x.Shrinker t = Seq.empty }
     static member Arr() =
      { new Arbitrary<Arr>() with
            override x.Generator = Gen.elements ArraysL
            override x.Shrinker t = Seq.empty }
"
let botL = @"

Arb.register<CustomGeneratorLarge>()

let checkMonotonicity1l (x:sigma, y:sigma, e:Edge) = (subsetOP (x, y))  ==> (lazy (subsetOP ((TF_Analysis (x, e)), (TF_Analysis (y, e)))))
let checkMonotonicity2l (x:sigma, y:sigma, e:Edge) = (supersetOP (x, y))  ==> (lazy (supersetOP ((TF_Analysis (x, e)), (TF_Analysis (y, e)))))

printfn ""Checking Monotonicity for Large sized programs""
Check.Quick checkMonotonicity1l
Check.Quick checkMonotonicity2l

"

let assemble par mn descr = (header par) + (customS mn descr) + botS + genM + (customM mn descr) + botM + genL + (customL mn descr) + botL

(*
    Needed:
    *(Gen Small sets)
    *Gen custom domain elements
    Monotonicity checks
    
    (Gen Medium sets)
    Gen custom domain elements
    Monotonicity checks
    
    (Gen Large sets)
    Gen custom domain elements
    Monotonicity checks
*)

(*
//old stuff
"

(*      GENERATORS BOT AND TOP      

let rec createAnalysisB list1 value =
    match list1 with
    | []    -> Map.empty
    | a::b  -> Map.fold (fun acc key value -> Map.add key value acc) (Map.empty.Add(a, value)) (createAnalysisB b value) 

let rec createAnalysisT list1 (valueL : 'a list) =
    match list1 with
    | []    -> Map.empty
    | a::b  -> Map.fold (fun acc key value -> Map.add key value acc) (Map.empty.Add(a, valueL.Head)) (createAnalysisT b valueL.Tail) 

let createAnalysis length xs ys= 
  gen { let! i = Gen.elements ys |> Gen.listOfLength length
        return (createAnalysisT xs i) }

type GeneratorsTop =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements Nodes //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements Variables 
          override x.Shrinker t = Seq.empty }
    static member Arr() =
      {new Arbitrary<Arr>() with
          override x.Generator = Gen.elements Arrays
          override x.Shrinker t = Seq.empty }
    static member Ident() =
      {new Arbitrary<Ident>() with
          override x.Generator = Gen.elements Identifiers 
          override x.Shrinker t = Seq.empty }
    static member Set() =
      {new Arbitrary<Set<'a>>() with
          override x.Generator = Gen.elements [ Set.ofList (Arb.generate<'a> |> Gen.sample 1000000000 500000)] //hopefully enough :D MAX INT ish
          override x.Shrinker t = Seq.empty }
" 

let rec customT m (mapDescriptor : Map<int, (string*string*string)>) = 
    match m with
    |   a when a=1  -> ""
    |   a when a>1  -> "    static member Map" + (a-1).ToString() + "() = 
      {new Arbitrary<Map"+ (a-1).ToString() + ">() with
          override x.Generator = createAnalysis " + fst3 (mapDescriptor.[a-1]) + ".Length " + fst3 (mapDescriptor.[a-1]) +  " ( Arb.generate<" + trd3 (mapDescriptor.[a-1]) + "> |> Gen.sample 1000000 1000000 |> Set.ofList |> Set.fold (fun l se -> se::l) [] )\n          override x.Shrinker t = Seq.empty }\n" + (customT (a-1) mapDescriptor)
    |   _  -> failwith "Error: Invalid map amount!"

let rec customB m (mapDescriptor : Map<int, (string*string*string)>) = 
    match m with
    |   a when a=1  -> ""
    |   a when a>1  -> "    static member Map" + (a-1).ToString() + "() = 
      {new Arbitrary<Map"+ (a-1).ToString() + ">() with
          override x.Generator = Gen.elements [createAnalysisB " + fst3 mapDescriptor.[a-1] + " (Arb.generate<" + trd3 mapDescriptor.[a-1] + "> |> Gen.sample 100 1).[0]]
          override x.Shrinker t = Seq.empty }\n" + (customB (a-1) mapDescriptor)
    |   _  -> failwith "Error: Invalid map amount!"

let middle = @"
Arb.register<GeneratorsTop>() |> ignore

let top = (Arb.generate<sigma> |> Gen.sample 10 1).[0]
// printfn ""Top as generated:\n%A"" top

type GeneratorsBot =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements Nodes //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements Variables 
          override x.Shrinker t = Seq.empty }
    static member Arr() =
      {new Arbitrary<Arr>() with
          override x.Generator = Gen.elements Arrays
          override x.Shrinker t = Seq.empty }
    static member Ident() =
      {new Arbitrary<Ident>() with
          override x.Generator = Gen.elements Identifiers
          override x.Shrinker t = Seq.empty }
    static member Set() =
      {new Arbitrary<Set<'a>>() with
          override x.Generator = Gen.elements [Set.empty]
          override x.Shrinker t = Seq.empty }
" 

let final = @"    static member AnalysisResult() = 
      {new Arbitrary<AnalysisResult>() with
          override x.Generator = Gen.elements [createAnalysisB Nodes (Arb.generate<sigma> |> Gen.sample 100 1).[0]]
          override x.Shrinker t = Seq.empty }

Arb.register<GeneratorsBot>() |> ignore

let bot = (Arb.generate<sigma> |> Gen.sample 0 1).[0]
// printfn ""Bot as generated:\n%A"" (Seq.toList bot)

"
let outputCode m mapDescriptor = header + (customT m mapDescriptor) + middle + (customB m mapDescriptor) + final
*)