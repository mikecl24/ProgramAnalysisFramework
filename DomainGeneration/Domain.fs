[<AutoOpen>]
module Domain

// Generated Code Section: Domain type
(*
Q -> P(Q)
*)

type Node = Node of int
type Var = Var of string


type Powerset1 = Node Set

type sigma = Powerset1

type AnalysisResult = Map<Node, sigma>

hi
