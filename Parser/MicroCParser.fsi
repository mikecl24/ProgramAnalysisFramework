// Signature file for parser generated by fsyacc
module MicroCParser
type token = 
  | COMMA
  | TRUE
  | FALSE
  | IF
  | FI
  | THEN
  | ELSE
  | WHILE
  | DO
  | OD
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | GT
  | GTEQ
  | LT
  | LTEQ
  | EQ
  | NEQ
  | ASSIGN
  | SKIP
  | PLUS
  | MULT
  | DIV
  | MINUS
  | AND
  | OR
  | SAND
  | SOR
  | NOT
  | EOF
  | NUM of (int)
  | VAR of (string)
type tokenId = 
    | TOKEN_COMMA
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_GT
    | TOKEN_GTEQ
    | TOKEN_LT
    | TOKEN_LTEQ
    | TOKEN_EQ
    | TOKEN_NEQ
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_PLUS
    | TOKEN_MULT
    | TOKEN_DIV
    | TOKEN_MINUS
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_SAND
    | TOKEN_SOR
    | TOKEN_NOT
    | TOKEN_EOF
    | TOKEN_NUM
    | TOKEN_VAR
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_command
    | NONTERM_aexp
    | NONTERM_bexp
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Statement list) 
