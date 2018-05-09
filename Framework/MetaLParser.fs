// Implementation file for parser generated by fsyacc
module MetaLParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 ".\MetaLParser.fsp"

open System
open DomainTypes



# 13 ".\MetaLParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | POWERSET
  | LBRA
  | RBRA
  | TFS
  | CART
  | VARSET
  | QSET
  | LSQ
  | RSQ
  | UNION
  | LCURL
  | RCURL
  | SCOL
  | EOF
  | ELEM of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_POWERSET
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_TFS
    | TOKEN_CART
    | TOKEN_VARSET
    | TOKEN_QSET
    | TOKEN_LSQ
    | TOKEN_RSQ
    | TOKEN_UNION
    | TOKEN_LCURL
    | TOKEN_RCURL
    | TOKEN_SCOL
    | TOKEN_EOF
    | TOKEN_ELEM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_domain
    | NONTERM_set
    | NONTERM_elist

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | POWERSET  -> 0 
  | LBRA  -> 1 
  | RBRA  -> 2 
  | TFS  -> 3 
  | CART  -> 4 
  | VARSET  -> 5 
  | QSET  -> 6 
  | LSQ  -> 7 
  | RSQ  -> 8 
  | UNION  -> 9 
  | LCURL  -> 10 
  | RCURL  -> 11 
  | SCOL  -> 12 
  | EOF  -> 13 
  | ELEM _ -> 14 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_POWERSET 
  | 1 -> TOKEN_LBRA 
  | 2 -> TOKEN_RBRA 
  | 3 -> TOKEN_TFS 
  | 4 -> TOKEN_CART 
  | 5 -> TOKEN_VARSET 
  | 6 -> TOKEN_QSET 
  | 7 -> TOKEN_LSQ 
  | 8 -> TOKEN_RSQ 
  | 9 -> TOKEN_UNION 
  | 10 -> TOKEN_LCURL 
  | 11 -> TOKEN_RCURL 
  | 12 -> TOKEN_SCOL 
  | 13 -> TOKEN_EOF 
  | 14 -> TOKEN_ELEM 
  | 17 -> TOKEN_end_of_input
  | 15 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_domain 
    | 3 -> NONTERM_domain 
    | 4 -> NONTERM_domain 
    | 5 -> NONTERM_set 
    | 6 -> NONTERM_set 
    | 7 -> NONTERM_set 
    | 8 -> NONTERM_set 
    | 9 -> NONTERM_set 
    | 10 -> NONTERM_elist 
    | 11 -> NONTERM_elist 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 17 
let _fsyacc_tagOfErrorTerminal = 15

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | POWERSET  -> "POWERSET" 
  | LBRA  -> "LBRA" 
  | RBRA  -> "RBRA" 
  | TFS  -> "TFS" 
  | CART  -> "CART" 
  | VARSET  -> "VARSET" 
  | QSET  -> "QSET" 
  | LSQ  -> "LSQ" 
  | RSQ  -> "RSQ" 
  | UNION  -> "UNION" 
  | LCURL  -> "LCURL" 
  | RCURL  -> "RCURL" 
  | SCOL  -> "SCOL" 
  | EOF  -> "EOF" 
  | ELEM _ -> "ELEM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | POWERSET  -> (null : System.Object) 
  | LBRA  -> (null : System.Object) 
  | RBRA  -> (null : System.Object) 
  | TFS  -> (null : System.Object) 
  | CART  -> (null : System.Object) 
  | VARSET  -> (null : System.Object) 
  | QSET  -> (null : System.Object) 
  | LSQ  -> (null : System.Object) 
  | RSQ  -> (null : System.Object) 
  | UNION  -> (null : System.Object) 
  | LCURL  -> (null : System.Object) 
  | RCURL  -> (null : System.Object) 
  | SCOL  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | ELEM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 3us; 65535us; 0us; 2us; 9us; 10us; 12us; 11us; 7us; 65535us; 0us; 8us; 5us; 6us; 9us; 8us; 12us; 8us; 16us; 13us; 19us; 14us; 20us; 15us; 2us; 65535us; 22us; 23us; 26us; 27us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 7us; 15us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 4us; 1us; 1us; 1us; 2us; 1us; 2us; 2us; 2us; 5us; 1us; 2us; 2us; 3us; 5us; 1us; 3us; 2us; 3us; 4us; 2us; 4us; 4us; 1us; 4us; 2us; 5us; 5us; 2us; 5us; 8us; 2us; 5us; 8us; 1us; 5us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 2us; 10us; 11us; 1us; 11us; 1us; 11us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 11us; 13us; 16us; 18us; 21us; 23us; 26us; 29us; 31us; 34us; 37us; 40us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 61us; 63us; |]
let _fsyacc_action_rows = 28
let _fsyacc_actionTableElements = [|5us; 32768us; 0us; 4us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 0us; 49152us; 2us; 32768us; 4us; 12us; 13us; 3us; 0us; 16385us; 1us; 32768us; 1us; 5us; 4us; 32768us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 2us; 32768us; 2us; 7us; 4us; 16us; 0us; 16386us; 2us; 32768us; 3us; 9us; 4us; 16us; 5us; 32768us; 0us; 4us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 0us; 16387us; 0us; 16388us; 5us; 32768us; 0us; 4us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 0us; 16389us; 2us; 32768us; 4us; 16us; 9us; 20us; 2us; 32768us; 4us; 16us; 8us; 21us; 4us; 32768us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 0us; 16390us; 0us; 16391us; 4us; 32768us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 4us; 32768us; 5us; 17us; 6us; 18us; 7us; 19us; 10us; 22us; 0us; 16392us; 1us; 32768us; 14us; 25us; 1us; 32768us; 11us; 24us; 0us; 16393us; 1us; 16394us; 12us; 26us; 1us; 32768us; 14us; 25us; 0us; 16395us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 6us; 7us; 10us; 11us; 13us; 18us; 21us; 22us; 25us; 31us; 32us; 33us; 39us; 40us; 43us; 46us; 51us; 52us; 53us; 58us; 63us; 64us; 66us; 68us; 69us; 71us; 73us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 4us; 3us; 3us; 3us; 1us; 1us; 5us; 3us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 16386us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 16391us; 65535us; 65535us; 16392us; 65535us; 65535us; 16393us; 65535us; 65535us; 16395us; |]
let _fsyacc_reductions ()  =    [| 
# 167 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : domain)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 176 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'domain)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 21 ".\MetaLParser.fsp"
                                             _1
                   )
# 21 ".\MetaLParser.fsp"
                 : domain));
# 187 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'set)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 ".\MetaLParser.fsp"
                                                               PowersetDom(_3) 
                   )
# 24 ".\MetaLParser.fsp"
                 : 'domain));
# 198 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'set)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'domain)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 ".\MetaLParser.fsp"
                                                               TotalFunctionSpaceDom(_1,_3) 
                   )
# 25 ".\MetaLParser.fsp"
                 : 'domain));
# 210 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'domain)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'domain)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 ".\MetaLParser.fsp"
                                                               CartesianDom(_1,_3) 
                   )
# 26 ".\MetaLParser.fsp"
                 : 'domain));
# 222 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'set)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'set)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 ".\MetaLParser.fsp"
                                                               CartesianSet(_1,_3) 
                   )
# 29 ".\MetaLParser.fsp"
                 : 'set));
# 234 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 ".\MetaLParser.fsp"
                                                               VARSet 
                   )
# 30 ".\MetaLParser.fsp"
                 : 'set));
# 244 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 ".\MetaLParser.fsp"
                                                               QSet 
                   )
# 31 ".\MetaLParser.fsp"
                 : 'set));
# 254 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'set)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'set)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 ".\MetaLParser.fsp"
                                                               UnionSet(_2,_4) 
                   )
# 32 ".\MetaLParser.fsp"
                 : 'set));
# 266 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'elist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 ".\MetaLParser.fsp"
                                                               ListSet(_2) 
                   )
# 33 ".\MetaLParser.fsp"
                 : 'set));
# 277 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 ".\MetaLParser.fsp"
                                                               Element(_1) 
                   )
# 36 ".\MetaLParser.fsp"
                 : 'elist));
# 288 ".\MetaLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'elist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 ".\MetaLParser.fsp"
                                                               LargerList(_1,_3) 
                   )
# 37 ".\MetaLParser.fsp"
                 : 'elist));
|]
# 301 ".\MetaLParser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 18;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : domain =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))