type state = Raw | Code | StringLiteral | IntLiteral | Symbol
type token =
  |TokRaw of string
  |TokStr of string
  |TokSym of string
  |TokNum of int
  |TokAdd
  |TokSub
  |TokMul
  |TokDiv
  |TokPow
  |TokLParen
  |TokRParen
  |TokLBracket
  |TokRBracket
  |TokLBrace
  |TokRBrace
  |TokLT
  |TokGT
  |TokColon
  |TokSemicolon
  |TokComma
  |TokDot
  |TokEquiv
  |TokEquals
  |TokNotEquiv
  |TokAddEquals
  |TokSubEquals
  |TokMulEquals
  |TokDivEquals
  |TokPowEquals
  |TokLTEquiv
  |TokGTEquiv
  |TokIncrement
  |TokDecrement

let string_of_token = function
  |TokRaw s -> "Raw"
  |TokStr s -> "String: "^s
  |TokSym s -> "Symbol: "^s
  |TokNum n -> "Number: "^(string_of_int n)
  |TokAdd -> "+"
  |TokSub -> "-"
  |TokMul -> "*"
  |TokDiv -> "/"
  |TokPow -> "^"
  |TokLParen -> "("
  |TokRParen -> ")"
  |TokLBracket -> "["
  |TokRBracket -> "]"
  |TokLBrace -> "{"
  |TokRBrace -> "}"
  |TokLT -> "<"
  |TokGT -> ">"
  |TokColon -> ":"
  |TokSemicolon -> ";"
  |TokComma -> ","
  |TokDot -> "."
  |TokEquiv -> "=="
  |TokEquals -> "="
  |TokNotEquiv -> "!="
  |TokAddEquals -> "+="
  |TokSubEquals -> "-="
  |TokMulEquals -> "*="
  |TokDivEquals -> "/="
  |TokPowEquals -> "^="
  |TokLTEquiv -> "<="
  |TokGTEquiv -> ">="
  |TokIncrement -> "++"
  |TokDecrement -> "--"

let explode s = List.init (String.length s) (String.get s)

let string_of_char = String.make 1

let lex p =
  let rec lex_i = function 
    (Raw, p, currentInt, currentString, tokens) -> (
      match p with
        |'{'::'%'::r -> lex_i (Code, r, currentInt, "", ((TokRaw currentString)::tokens))
        |'{'::'!'::r -> lex_i (Code, r, currentInt, "", TokLParen::(TokSym "print")::((TokRaw currentString)::tokens))
        |hd::tl -> lex_i (Raw, tl, currentInt, currentString ^ (string_of_char hd), tokens)
        |[]->((TokRaw currentString)::tokens)
    )
    |(Code, p, currentInt, currentString, tokens) -> (
      match p with
        |'='::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokEquiv::tokens)
        |'!'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokNotEquiv::tokens)
        |'<'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokLTEquiv::tokens)
        |'>'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokGTEquiv::tokens)
        |'+'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokAddEquals::tokens)
        |'+'::'+'::tl -> lex_i (Code, tl, currentInt, currentString, TokIncrement::tokens)
        |'-'::'-'::tl -> lex_i (Code, tl, currentInt, currentString, TokDecrement::tokens)
        |'-'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokSubEquals::tokens)
        |'*'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokMulEquals::tokens)
        |'/'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokDivEquals::tokens)
        |'^'::'='::tl -> lex_i (Code, tl, currentInt, currentString, TokPowEquals::tokens)
        |'%'::'}'::tl -> lex_i (Raw, tl, currentInt, currentString, tokens)
        |'!'::'}'::tl -> lex_i (Raw, tl, currentInt, currentString, TokRParen::tokens)
        |'+'::tl -> lex_i (Code, tl, currentInt, currentString, TokAdd::tokens)
        |'-'::tl -> lex_i (Code, tl, currentInt, currentString, TokSub::tokens)
        |'*'::tl -> lex_i (Code, tl, currentInt, currentString, TokMul::tokens)
        |'/'::tl -> lex_i (Code, tl, currentInt, currentString, TokDiv::tokens)
        |'^'::tl -> lex_i (Code, tl, currentInt, currentString, TokPow::tokens)
        |'('::tl -> lex_i (Code, tl, currentInt, currentString, TokLParen::tokens)
        |')'::tl -> lex_i (Code, tl, currentInt, currentString, TokRParen::tokens)
        |'['::tl -> lex_i (Code, tl, currentInt, currentString, TokLBracket::tokens)
        |']'::tl -> lex_i (Code, tl, currentInt, currentString, TokRBracket::tokens)
        |'{'::tl -> lex_i (Code, tl, currentInt, currentString, TokLBrace::tokens)
        |'}'::tl -> lex_i (Code, tl, currentInt, currentString, TokRBrace::tokens)
        |'<'::tl -> lex_i (Code, tl, currentInt, currentString, TokLT::tokens)
        |'>'::tl -> lex_i (Code, tl, currentInt, currentString, TokGT::tokens)
        |':'::tl -> lex_i (Code, tl, currentInt, currentString, TokColon::tokens)
        |';'::tl -> lex_i (Code, tl, currentInt, currentString, TokSemicolon::tokens)
        |'.'::tl -> lex_i (Code, tl, currentInt, currentString, TokDot::tokens)
        |','::tl -> lex_i (Code, tl, currentInt, currentString, TokComma::tokens)
        |'='::tl -> lex_i (Code, tl, currentInt, currentString, TokEquals::tokens)
        |'"'::tl -> lex_i (StringLiteral, tl, currentInt, currentString, tokens)
        |'0'..'9'::_ -> lex_i (IntLiteral, p, currentInt, currentString, tokens)
        |('a'..'z'|'A'..'Z')::_ -> lex_i (Symbol, p, currentInt, currentString, tokens)
        |hd::tl -> lex_i (Code, tl, currentInt, currentString, tokens)
        |[]->tokens
    )
    |(Symbol, p, currentInt, currentString, tokens) -> (
      match p with 
        |(('a'..'z'|'A'..'Z'|'_') as hd)::tl -> lex_i (Symbol, tl, currentInt, currentString ^ Char.escaped hd, tokens)
        |[] -> (TokSym currentString)::tokens
        |p -> lex_i (Code, p, currentInt, "", (TokSym currentString)::tokens)
    )
    |(StringLiteral, p, currentInt, currentString, tokens) -> (
      match p with
        |'\\'::'"'::tl -> lex_i (StringLiteral, tl, currentInt, currentString ^ ("\""), tokens)
        |'"'::tl -> lex_i (Code, tl, currentInt, "", (TokStr currentString)::tokens)
        |hd::tl -> lex_i (StringLiteral, tl, currentInt, currentString ^ (string_of_char hd), tokens)
        |[] -> (TokStr currentString)::tokens
    )
    |(IntLiteral, p, currentInt, currentString, tokens) -> (
      match p with 
        |('0'..'9' as hd)::tl -> lex_i (IntLiteral, tl, currentInt*10 + int_of_char hd - 0x30, currentString, tokens)
        |[] -> (TokNum currentInt)::tokens
        |p -> lex_i (Code, p, 0, currentString, (TokNum currentInt)::tokens)
    )
  in 
  let result = lex_i (Raw, explode p, 0, "", []) |> List.rev in
  print_endline "Finished Lexing...";
  result

let rec print_tokens = function
  (hd::tl) -> (
    string_of_token hd |> print_endline;
    print_tokens tl
  )
  |[] -> ()