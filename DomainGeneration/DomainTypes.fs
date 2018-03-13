module DomainTypes


type elemlist = 
  | Element of string
  | LargerList of (string*elemlist)

type set = 
  | CartesianSet of (set*set)
  | CartesianListSet of set list
  | QSet
  | VARSet
  | UnionSet of (set*set)
  | ListSet of elemlist


type domain =
  | PowersetDom of set
  | TotalFunctionSpaceDom of (set*domain)
  | CartesianDom of (domain*domain)
  | CartesianListDom of domain list
