[<AutoOpen>]
module GenerateGenerators

let header = @"
[<AutoOpen>]
module LatticeStates
(*      GENERATORS BOT AND TOP      *)

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
          override x.Generator = Gen.elements Variables //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }
    static member Arr() =
      {new Arbitrary<Arr>() with
          override x.Generator = Gen.elements Arrays //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }
    static member Ident() =
      {new Arbitrary<Ident>() with
          override x.Generator = Gen.elements Identifiers //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }
    static member Set() =
      {new Arbitrary<Set<'a>>() with
          override x.Generator = Gen.elements [ Set.ofList (Arb.generate<'a> |> Gen.sample 1000000000 500000)] //hopefully enough :D MAX INT ish
          override x.Shrinker t = Seq.empty }
" 

//Replace me!!
let rec customT m (mapDescriptor : Map<int, (string*string*string)>) = 
    match m with
    |   a when a=1  -> ""
    |   a when a>1  -> "    static member Map" + (a-1).ToString() + "() = 
      {new Arbitrary<Map"+ (a-1).ToString() + ">() with
          override x.Generator = createAnalysis " + fst3 (mapDescriptor.[a-1]) + ".Length " + fst3 (mapDescriptor.[a-1]) +  " ( Arb.generate<" + trd3 (mapDescriptor.[a-1]) + "> |> Gen.listOfLength " + fst3 (mapDescriptor.[a-1]) + ".Length |> Gen.sample 1000000 1).[0] \n          override x.Shrinker t = Seq.empty }\n " + (customT (a-1) mapDescriptor)
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
          override x.Generator = Gen.elements Variables //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }
    static member Arr() =
      {new Arbitrary<Arr>() with
          override x.Generator = Gen.elements Arrays //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
          override x.Shrinker t = Seq.empty }
    static member Ident() =
      {new Arbitrary<Ident>() with
          override x.Generator = Gen.elements Identifiers //Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString x) -> Var (x (*+ string((Arb.generate<Char> |> Gen.sample 1 1).[0]*) ) )
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
