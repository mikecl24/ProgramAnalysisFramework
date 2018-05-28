[<AutoOpen>]
module DomainTypes


type elemlist = 
  | Element of string
  | LargerList of (string * elemlist)

type sset = 
  | QSet
  | VARSet
  | ARRSet
  | IDENTSet

type set = 
  | SsetSet of sset
  | CartesianSet of (set * set)
  | CartesianListSet of set list
  | TotalFunctionSpaceSet of (sset * set)
  | PowersetSet of set
  | UnionSet of (set * set)
  | ListSet of elemlist
  | ElemList of string list
  | INTSet
  | STRSet

type domain =
  | PowersetDom of set
  | TotalFunctionSpaceDom of (sset * domain)
  | CartesianDom of (domain * domain)
  | CartesianListDom of domain list
  | SetDom of set