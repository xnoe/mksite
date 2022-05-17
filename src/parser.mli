type ast =
    OpInsertRaw of string
  | OpSymb of string
  | OpString of string
  | OpNumber of int
  | OpList of ast list
  | OpDict of ast list
  | OpDictMapping of ast * ast
  | OpAdd of ast * ast
  | OpSub of ast * ast
  | OpMul of ast * ast
  | OpDiv of ast * ast
  | OpPow of ast * ast
  | OpNegate of ast
  | OpIfElse of ast * ast * ast
  | OpFor of ast * ast * ast * ast
  | OpWhile of ast * ast
  | OpStatementList of ast list
  | OpBlock of ast
  | OpFunDef of ast * ast * ast
  | OpFunCall of ast * ast
  | OpArgList of ast list
  | OpParamList of ast list
  | OpAssign of ast * ast
  | OpLetAssign of ast * ast
  | OpEquiv of ast * ast
  | OpNotEquiv of ast * ast
  | OpLT of ast * ast
  | OpGT of ast * ast
  | OpLTE of ast * ast
  | OpGTE of ast * ast
  | OpIncYield of ast
  | OpDecYield of ast
  | OpYieldInc of ast
  | OpYieldDec of ast
  | OpReturn of ast
  | OpDot of ast * ast
  | OpAccess of ast * ast
  | OpProgram of ast
  | OpInclude of ast
  | OpNop
  | OpRep of ast * ast * ast
  | OpForEach of ast * ast * ast
  | OpForEachKV of ast * ast * ast * ast
val parse : Lexer.token list -> ast
