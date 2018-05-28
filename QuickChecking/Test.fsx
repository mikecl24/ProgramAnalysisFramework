(*#r "FsCheck.dll"

type Var = Var of string
type Arr = Arr of string
type Node = Node of int

type Identifier =
| Var1 of Var
| Arr1 of Arr

#load "Domain.fs"

open FsCheck

let Nodes = [Node 0; Node 1; Node 2; Node 3]
let Variables = [Var "x"; Var "y"; Var "z"]
let Arrays = [Arr "A"; Arr "B"; Arr "C"]

//generate ident

let Identifiers =
 List.map (fun x -> Var1 x) Variables @
 List.map (fun x -> Arr1 x) Arrays

printfn "%A" Identifiers


let rec createAnalysis list1 value =
    match list1 with
    | []    -> Map.empty
    | a::b  -> Map.fold (fun acc key value -> Map.add key value acc) (Map.empty.Add(a, value)) (createAnalysis b value) 


type GeneratorsTop =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements Nodes //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements Variables //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x + string((Arb.generate<Char> |> Gen.sample 1 1).[0] ) )
          override x.Shrinker t = Seq.empty }
    static member Arr() =
      {new Arbitrary<Arr>() with
          override x.Generator = Gen.elements Arrays //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x + string((Arb.generate<Char> |> Gen.sample 1 1).[0] ) )
          override x.Shrinker t = Seq.empty }
    static member Set() =
      {new Arbitrary<Set<'a>>() with
          override x.Generator = Gen.elements [ Set.ofList (Arb.generate<'a> |> Gen.sample 1000000000 500000)] //hopefully enough :D MAX INT ish
          override x.Shrinker t = Seq.empty }

    static member Map1() = 
      {new Arbitrary<Map1>() with
          override x.Generator = Gen.elements [createAnalysis Identifiers (Arb.generate<'a> |> Gen.sample 100 1).[0]]
          override x.Shrinker t = Seq.empty }
    static member AnalysisResult() = 
      {new Arbitrary<AnalysisResult>() with
          override x.Generator = Gen.elements [createAnalysis Nodes (Arb.generate<sigma> |> Gen.sample 100 1).[0]]
          override x.Shrinker t = Seq.empty }

Arb.register<GeneratorsTop>()
let top = (Arb.generate<AnalysisResult> |> Gen.sample 0 1).[0]
printfn "Top as generated:\n%A" top


type GeneratorsBot =
    static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Gen.elements Nodes //Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }
    static member Var() =
      {new Arbitrary<Var>() with
          override x.Generator = Gen.elements Variables //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x + string((Arb.generate<Char> |> Gen.sample 1 1).[0] ) )
          override x.Shrinker t = Seq.empty }
    static member Arr() =
      {new Arbitrary<Arr>() with
          override x.Generator = Gen.elements Arrays //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x + string((Arb.generate<Char> |> Gen.sample 1 1).[0] ) )
          override x.Shrinker t = Seq.empty }
    static member Set() =
      {new Arbitrary<Set<'a>>() with
          override x.Generator = Gen.elements [Set.empty]
          override x.Shrinker t = Seq.empty }

    static member Map1() = 
      {new Arbitrary<Map1>() with
          override x.Generator = Gen.elements [createAnalysis Identifiers (Arb.generate<'a> |> Gen.sample 100 1).[0]]
          override x.Shrinker t = Seq.empty }
    static member AnalysisResult() = 
      {new Arbitrary<AnalysisResult>() with
          override x.Generator = Gen.elements [createAnalysis Nodes (Arb.generate<sigma> |> Gen.sample 100 1).[0]]
          override x.Shrinker t = Seq.empty }

Arb.register<GeneratorsBot>()

let bot = (Arb.generate<AnalysisResult> |> Gen.sample 0 1).[0]
printfn "Top as generated:\n%A" bot
*)

let mutable x = 1
let test num = num+1
let test2 = test x

printfn "%A" (test2)

x <- 3

printfn "%A" (test2)