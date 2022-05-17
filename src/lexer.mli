type state = Raw | Code | StringLiteral | IntLiteral | Symbol
type token =
    TokRaw of string
  | TokStr of string
  | TokSym of string
  | TokNum of int
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokPow
  | TokLParen
  | TokRParen
  | TokLBracket
  | TokRBracket
  | TokLBrace
  | TokRBrace
  | TokLT
  | TokGT
  | TokColon
  | TokSemicolon
  | TokComma
  | TokDot
  | TokEquiv
  | TokEquals
  | TokNotEquiv
  | TokAddEquals
  | TokSubEquals
  | TokMulEquals
  | TokDivEquals
  | TokPowEquals
  | TokLTEquiv
  | TokGTEquiv
  | TokIncrement
  | TokDecrement
val string_of_token : token -> string
val explode : string -> char list
val string_of_char : char -> string
val lex : string -> token list
val print_tokens : token list -> unit
