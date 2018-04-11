#r "FsCheck.dll"

open FsCheck
open System
open System.Threading
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic
open Prop

#load "Domain.fs"                   // Domain Specification Generated code
open Domain                         // Q -> Domain variable + types

let subset_pw (dom1: 'a Set, dom2: 'a Set) = Set.isSubset dom1 dom2
let superset_pw (dom1, dom2) =  Set.isSuperset dom1 dom2

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

(*
let add x y = x + y
let revRevIsComm ((x:int),(y:int)) = add x y = add y x
Check.Quick revRevIsComm
*)


// before redefining generator
printfn "%A" (Arb.generate<Node> |> Gen.sample 10 30)

type Generators =
    static member Node() =
        Arb.generate<int>
        |> Gen.map (fun i -> if i>=0 then i else -i)
        |> Arb.fromGen
 
type MyGenerators =
  static member Node() =
      {new Arbitrary<Node>() with
          override x.Generator = Arb.generate<int> |> Gen.map (fun i -> if i>=0 then Node i else Node (-i-1) )
          override x.Shrinker t = Seq.empty }

Arb.register<MyGenerators>()

// after redefining generator
printfn "%A" (Arb.generate<Node> |> Gen.sample 10 30)

(*
Arb.generate<int> |> Gen.map (fun i -> if i>=0 then i else -i) |> Gen.sample 10 10

Gen.oneof [ gen { return true }; gen { return false } ]
*)