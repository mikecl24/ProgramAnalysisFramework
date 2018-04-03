module Domain

// Generated Code Section: Domain type
(*
Q -> P( [Q U [VAR U {QTes}] ] )
*)

type Node = Node of int
type Var = Var of string


test

type Union1 = 
    | Q1 of Node 
    | Unimplemented evaluation of union in union


type Powerset1 = Powerset1 of Union1 Set

type AnalysisResult = AnalysisResult of Map<Node,Powerset1>