[<AutoOpen>]
module DomainTypes
open System
open System.Runtime.Serialization.Formatters
open System.Runtime.Serialization


type elemlist = 
  | Element of string
  | LargerList of (string * elemlist)

type set = 
  | CartesianSet of (set * set)
  | CartesianListSet of set list
  | QSet
  | VARSet
  | ARRSet
  | UnionSet of (set * set)
  | ListSet of elemlist
  | ElemList of string list
  | PowersetSet of set
  | TotalFunctionSpaceSet of (set * set)


type domainM =
  | PowersetDom of set
  | TotalFunctionSpaceDom of (set * domainM)
  | CartesianDom of (domainM * domainM)
  | CartesianListDom of domainM list

type iset = 
  | CartesianSet_G of (iset * iset)
  | VARSet_G
  | ARRSet_G
  | QSet_G
  | INTSet_G
  | STRSet_G
  | UnionSet_G of (iset * iset)
  | ListSet_G of elemlist
  | ElemList_G of string list
  | PowersetSet_G of set

type domainG = 
  | TotalFunctionSpaceDom_G of (set * domainG)
  | ISet of iset

type domain = 
  | DomainMetaL of domainM
  | DomainGraph of domainG
