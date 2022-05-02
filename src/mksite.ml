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

let string_of_char = function 
  |'\n' -> "\n"
  |'\t' -> "\t"
  |'\b' -> "\b"
  |c -> Char.escaped c

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
        (('a'..'z'|'A'..'Z'|'_') as hd)::tl -> lex_i (Symbol, tl, currentInt, currentString ^ Char.escaped hd, tokens)
        |[] -> (TokSym currentString)::tokens
        |p -> lex_i (Code, p, currentInt, "", (TokSym currentString)::tokens)
    )
    |(StringLiteral, p, currentInt, currentString, tokens) -> (
      match p with
        '"'::tl -> lex_i (Code, tl, currentInt, "", (TokStr currentString)::tokens)
        |hd::tl -> lex_i (StringLiteral, tl, currentInt, currentString ^ (string_of_char hd), tokens)
        |[] -> (TokStr currentString)::tokens
    )
    |(IntLiteral, p, currentInt, currentString, tokens) -> (
      match p with 
        ('0'..'9' as hd)::tl -> lex_i (IntLiteral, tl, currentInt*10 + int_of_char hd - 0x30, currentString, tokens)
        |[] -> (TokNum currentInt)::tokens
        |p -> lex_i (Code, p, 0, currentString, (TokNum currentInt)::tokens)
    )
  in lex_i (Raw, explode p, 0, "", []) |> List.rev

let rec print_tokens = function
  (hd::tl) -> (
    string_of_token hd |> print_endline;
    print_tokens tl
  )
  |[] -> ()
(*
  Mksite Grammar:

  literal := Number
           | String
           | List
           | Dict

  term := LParen <expr> RParen
        | <function_call>
        | <symbol>
        | <literal>
  
  access := <term> {LBracket <term> RBracket}*
  
  dot := <access> {Dot <access>}*

  incdec := (Inc|Dec) <dot>
          | <dot> (Inc|Dec)?

  pow := <term> {Pow <term>}*

  factor := <pow> {(Mul|Div) <pow>}*

  math_expr := <factor> {(Add|Sub) <factor>}*

  bool_expr := <math_expr> Equiv <math_expr>
             | <math_expr> NotEquiv <math_expr>
             | <math_expr> LT <math_expr>
             | <math_expr> GT <math_expr>
             | <math_expr> GTE <math_expr>
             | <math_expr> LTE <math_expr>
             | <math_expr>

  assign_expr := <symbol> (Equals|AddEquals|SubEquals|MulEquals|DivEquals|PowEquals) <expr>
  
  expr := <assign_expr>
        | <bool_expr>

  expr_statement := <expr>

  arglist := ()
           | <symbol> {Comma <symbol>}*
  
  paramlist := ()
             | <expr> {Comma <expr>}*
  
  function_call := <symbol> LParen <paramlist> RParen

  block := LBrace <statement_list> RBrace

  function_def := Fn <symbol> LParen <arglist> RParen <block>

  for_loop := For LParen <expr_statement> Semicolon <expr_statement> Semicolon <expr_statement> RParen <block>

  if_statement := If LParen <expr_statement> RParen <block> (Else <statement>)?
  
  while_loop := While LParen <expr_statement> RParen <block>

  return_statement := Return <expr>

  let_statement := Let <assign_expr>

  include_statement := Include <literal>

  statement := function_def
             | expr_statement
             | for_loop
             | while_loop
             | if_statement
             | return_statement
             | let_statement
             | include_statement
             | <block>
             | Raw
  
  statement_list := {<statement> Semicolon}*
  
  program := <statement_list>
*)

type ast =
  |OpInsertRaw of string
  |OpSymb of string
  |OpString of string
  |OpNumber of int
  |OpList of ast list
  |OpDict of ast list
  |OpDictMapping of ast * ast
  |OpAdd of ast * ast
  |OpSub of ast * ast
  |OpMul of ast * ast
  |OpDiv of ast * ast
  |OpPow of ast * ast
  |OpNegate of ast
  |OpIfElse of ast * ast * ast
  |OpFor of ast * ast * ast * ast
  |OpWhile of ast * ast
  |OpStatementList of ast list
  |OpBlock of ast 
  |OpFunDef of ast * ast * ast
  |OpFunCall of ast * ast
  |OpArgList of ast list
  |OpParamList of ast list
  |OpAssign of ast * ast
  |OpLetAssign of ast * ast
  |OpEquiv of ast * ast
  |OpNotEquiv of ast * ast
  |OpLT of ast * ast
  |OpGT of ast * ast
  |OpLTE of ast * ast
  |OpGTE of ast * ast
  |OpIncYield of ast
  |OpDecYield of ast
  |OpYieldInc of ast
  |OpYieldDec of ast
  |OpReturn of ast
  |OpDot of ast * ast
  |OpAccess of ast * ast
  |OpProgram of ast
  |OpInclude of ast
  |OpNop

let parse t = 
  let tokens = ref t in

  let syntax_error f e = Failure ("Syntax Error! Found `" ^ (string_of_token f) ^ "`, expected `" ^ (string_of_token e) ^ "`") in

  let eat_raw () =
    tokens := (match !tokens with
      |(TokRaw _)::hd -> hd
      |[t] -> raise (syntax_error t (TokRaw ""))
      |_->raise (Failure("No More Tokens!")));
  in

  let eat_string () =
    tokens := (match !tokens with
      |(TokStr _)::hd -> hd
      |[t] -> raise (syntax_error t (TokStr ""))
      |_->raise (Failure("No More Tokens!")));
  in

  let eat_symbol () =
    tokens := (match !tokens with
      |(TokSym _)::hd -> hd
      |[t] -> raise (syntax_error t (TokSym ""))
      |_->raise (Failure("No More Tokens!")));
  in

  let eat_number () =
    tokens := (match !tokens with
      |(TokNum _)::hd -> hd
      |[t] -> raise (syntax_error t (TokNum 0))
      |_->raise (Failure("No More Tokens!")));
  in

  let peek () =
    match !tokens with
      |hd::_ -> Some hd
      |[] -> None
  in

  let peek2 () =
    match !tokens with
      |_::hd::_ -> Some hd
      |_ -> None
  in

  let peek3 () =
    match !tokens with
      |_::_::hd::_ -> Some hd
      |_ -> None
  in

  let unwrap = function 
    |Some o -> o 
    |_ -> raise (Failure "Unwrap failed!")
  in

  let eat s =
    if peek () <> Some s then raise (syntax_error (peek () |> unwrap) s);
    match !tokens with 
      |hd::tl -> tokens := tl
      |_ -> raise (Failure "No more tokens!")
  in
  
  let is_raw () = match peek () with |Some TokRaw _->true|_->false in
  let is_str () = match peek () with |Some TokStr _->true|_->false in
  let is_sym () = match peek () with |Some TokSym _->true|_->false in
  let is_num () = match peek () with |Some TokNum _->true|_->false in
  
  let rec literal () =
    match !tokens with
      |(TokNum n)::_ -> (eat_number (); OpNumber n)
      |(TokStr s)::_ -> (eat_string (); OpString s)
      |(TokLBracket)::_ -> (eat TokLBracket; let OpParamList v = paramlist () in eat TokRBracket; OpList v)
      |(TokLBrace)::_ -> (
        eat TokLBrace;
        if (peek () |> unwrap) = TokRBrace then (eat TokRBrace; OpDict [])
        else
        let rec dict_mapping_i acc =
          match (peek () |> unwrap) with
            |TokComma -> (eat TokComma; let lhs = literal() in eat TokColon; let rhs = expr () in dict_mapping_i ((OpDictMapping (lhs, rhs))::acc))
            |_ -> eat TokRBrace; acc
        in
        let lhs = literal() in
        eat TokColon; 
        let rhs = expr () in 
        OpDict (dict_mapping_i [OpDictMapping (lhs, rhs)])
      )
      |_ -> OpNop
  and term () =
    match !tokens with 
      |TokLParen::_ -> (eat TokLParen; let e = expr () in eat TokRParen; e)
      |(TokSym s)::TokLParen::_ -> function_call ()
      |(TokSym s)::_ -> (eat_symbol (); OpSymb s)
      |_ -> literal ()
  and access () =
    let rec access_i node = 
      match (peek () |> unwrap) with
        |TokLBracket -> (eat TokLBracket; let t = term() in eat TokRBracket; access_i (OpAccess(node, t)))
        |_ -> node
    in access_i (term ())
  and dot () = 
    let rec dot_i node = 
      match (peek () |> unwrap) with
        |TokDot -> (eat TokDot; dot_i (OpDot(node, access ())))
        |_ -> node
    in dot_i (access ())
  and incdec () =
    match (peek () |> unwrap) with
      |TokIncrement -> (eat TokIncrement; OpIncYield(dot()))
      |TokDecrement -> (eat TokDecrement; OpDecYield(dot()))
      | _ -> (
        let t = dot () in
        match (peek () |> unwrap) with
          | TokIncrement -> (eat TokIncrement; OpYieldInc(t))
          | TokDecrement -> (eat TokDecrement; OpYieldDec(t))
          | _ -> t
      )
  and pow () = 
    let rec pow_i node = 
      match (peek () |> unwrap) with
        |TokPow -> (eat TokPow; pow_i (OpPow(node, incdec())))
        |_ -> node
    in pow_i (incdec ())
  and factor () =
    let rec factor_i node = 
      match (peek () |> unwrap) with
        |TokMul -> (eat TokMul; factor_i (OpMul(node, pow())))
        |TokDiv -> (eat TokDiv; factor_i (OpDiv(node, pow())))
        |_ -> node
    in factor_i (pow ())
  and math_expr () =
    let rec expr_i node = 
      match (peek () |> unwrap) with
        |TokAdd -> (eat TokAdd; expr_i (OpAdd(node, factor())))
        |TokSub -> (eat TokSub; expr_i (OpSub(node, factor())))
        |_ -> node
    in expr_i (factor ())
  and bool_expr () =
    let rec bool_expr_i node = 
      match (peek () |> unwrap) with
        |TokEquiv -> (eat TokEquiv; bool_expr_i (OpEquiv(node, math_expr())))
        |TokNotEquiv -> (eat TokNotEquiv; bool_expr_i (OpNotEquiv(node, math_expr())))
        |TokLT -> (eat TokLT; bool_expr_i (OpLT(node, math_expr())))
        |TokGT -> (eat TokGT; bool_expr_i (OpGT(node, math_expr())))
        |TokLTEquiv -> (eat TokLTEquiv; bool_expr_i (OpLTE(node, math_expr())))
        |TokGTEquiv -> (eat TokGTEquiv; bool_expr_i (OpGTE(node, math_expr())))
        |_ -> node
    in bool_expr_i (math_expr ())
  and assign_expr () =
    match !tokens with
      |(TokSym s)::TokEquals::_ -> (eat_symbol (); eat TokEquals; OpAssign(OpSymb s, expr ()))
      |(TokSym s)::TokAddEquals::_ -> (eat_symbol (); eat TokAddEquals; OpAssign(OpSymb s, OpAdd(OpSymb s, expr ())))
      |(TokSym s)::TokSubEquals::_ -> (eat_symbol (); eat TokSubEquals; OpAssign(OpSymb s, OpSub(OpSymb s, expr ())))
      |(TokSym s)::TokMulEquals::_ -> (eat_symbol (); eat TokMulEquals; OpAssign(OpSymb s, OpMul(OpSymb s, expr ())))
      |(TokSym s)::TokDivEquals::_ -> (eat_symbol (); eat TokDivEquals; OpAssign(OpSymb s, OpDiv(OpSymb s, expr ())))
      |(TokSym s)::TokPowEquals::_ -> (eat_symbol (); eat TokPowEquals; OpAssign(OpSymb s, OpPow(OpSymb s, expr ())))
  and expr () =
    match !tokens with 
      |(TokSym s)::(TokEquals|TokAddEquals|TokSubEquals|TokMulEquals|TokDivEquals|TokPowEquals)::_ -> assign_expr ()
      |_ -> bool_expr ()
  and expr_statement () = 
    expr ()
  and arglist () =
    if peek () = Some TokRParen then 
      OpArgList []
    else (
      let rec arglist_i l = match !tokens with
        |(TokSym s)::TokComma::_ -> (eat_symbol (); eat TokComma; arglist_i ((OpSymb s)::l))
        |(TokSym s)::_ -> (eat_symbol(); OpArgList ((OpSymb s)::l))
        |_ -> OpArgList (List.rev l)
      in arglist_i []
    )
  and paramlist () =
    if peek () = Some TokRParen then
      OpParamList []
    else (
      let rec paramlist_i l = match peek() with
        |Some TokComma -> (eat TokComma; paramlist_i ((expr ())::l))
        |_ -> OpParamList (List.rev l)
      in paramlist_i [expr ()]
    )
  and function_call () = 
    let Some (TokSym s) = peek () in
    eat_symbol ();
    eat TokLParen;
    let node = OpFunCall(OpSymb s, paramlist()) in
    eat TokRParen;
    node
  and block () = 
    eat TokLBrace;
    let node = ref [] in
    while peek () <> Some TokRBrace do
      node := !node @ [statement ()]
    done;
    eat TokRBrace;
    OpBlock (OpStatementList !node)
  and function_def () =
    match !tokens with
      |TokSym "fn"::(TokSym s)::_ -> (
        eat_symbol ();
        eat_symbol ();
        eat TokLParen;
        let args = arglist () in
        eat TokRParen;
        let block = block () in
        OpFunDef (OpSymb s, args, block)
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Fn"))
  and for_loop () =
    match !tokens with 
      |TokSym "for"::_ -> (
        eat_symbol ();
        eat TokLParen;
        let init = expr_statement () in
        eat TokSemicolon;
        let compare = expr_statement () in
        eat TokSemicolon;
        let after = expr_statement () in
        eat TokRParen;
        let block = block () in
        OpFor (init, compare, after, block)
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "For"))
  and if_statement () = 
    match !tokens with
      |TokSym "if"::_ -> (
        eat_symbol ();
        eat TokLParen;
        let compare = expr_statement () in
        eat TokRParen;
        let block = block () in
        match peek () with
          | Some (TokSym "else") -> (eat_symbol (); OpIfElse (compare, block, statement ()))
          | _ -> OpIfElse (compare, block, OpStatementList [])
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "If"))
  and while_loop () = 
    match !tokens with
      |TokSym "while"::_ -> (
        eat_symbol ();
        eat TokLParen;
        let compare = expr_statement () in
        eat TokRParen;
        let block = block () in
        OpWhile (compare, block);
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "While"))
  and return_statement () =
    match !tokens with
      |TokSym "return"::_ -> (
        eat_symbol ();
        OpReturn (expr ())
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Return"))
  and let_statement () =
    match !tokens with
      |(TokSym "let")::_ -> (
        eat_symbol ();
        let OpAssign (lhs, rhs) = assign_expr () in
        OpLetAssign (lhs, rhs)
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Let"))
  and include_statement () =
    match !tokens with
      |(TokSym "include")::(TokStr s)::_ -> (
        eat_symbol ();
        eat_symbol ();
        OpInclude (OpString s)
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Include"))
  and statement () =
    match (peek () |> unwrap) with
      |TokSym "fn" -> function_def ()
      |TokSym "if" -> if_statement ()
      |TokSym "for" -> for_loop ()
      |TokSym "while" -> while_loop ()
      |TokSym "return" -> return_statement ()
      |TokSym "let" -> let_statement ()
      |TokSym "include" -> include_statement ()
      |TokLBrace -> block ()
      |TokRaw r -> (eat_raw (); OpInsertRaw r)
      |_->expr_statement()
  and statement_list () =
    let statementlist = ref [] in
    while !tokens <> [] do
      statementlist := !statementlist @ [statement ()]
    done;
    OpStatementList !statementlist
  and program () =
    OpProgram (statement_list ())
  in program ()

type value = 
  |ValNumber of int
  |ValString of string
  |ValBool of bool
  |ValFunction of ast * ast
  |ValReturn of value
  |ValScope of (string, value) Hashtbl.t list
  |ValList of value list
  |ValDict of (value, value) Hashtbl.t
  |ValNone

let int_of_valnumber = function
  |ValNumber n -> n
  |_ -> raise (Failure "Invalid type")
let string_of_valstring = function
  |ValString s -> s
  |_ -> raise (Failure "Invalid type")
let bool_of_valbool = function
  |ValBool b -> b
  |_ -> raise (Failure "Invalid type")

let pow x = function
  |0 -> 1
  |1 -> x
  |y -> (
    let rec pow_i acc = function
      |1 -> acc
      |n -> pow_i (acc*x) (n-1)
    in pow_i x y
  )

let rec eval ?(path_prefix="") ?(default_assignments=[Hashtbl.create 100]) ast =
  let acc = ref "" in
  let rec truthy = function
    |ValNumber n when n <> 0 -> true
    |ValNumber _ -> false
    |ValString s when s <> "" -> true
    |ValString _ -> false
    |ValBool b -> b
    |ValFunction _ -> true
    |ValNone -> false
    |_ -> false
  and get_assigned name assignments =
    let rec get_assigned_i (hd::tl) =
      match Hashtbl.find_opt hd name with
        | Some v -> v
        | None -> (
          if tl == [] then ValNone
          else get_assigned_i tl
        )
    in get_assigned_i assignments
  and set_assigned ?(must_exist=true) name rhs assignments =
    let rec attempt_replace (hd::tl) =
      if not must_exist then false else (
        match Hashtbl.find_opt hd name with
          | Some v -> (Hashtbl.replace hd name rhs; true)
          | None -> (
            if tl == [] then false
            else attempt_replace tl
          )
      )
    in
    if attempt_replace assignments <> true then (
      if must_exist then (
        raise (Failure ("Tried to assign variable " ^ name ^ " (doesn't exist)"))
      ) else (
        let (hd::_) = assignments in
        Hashtbl.add hd name rhs
      )
    )
  and eval_i assignments = function
    |OpInsertRaw r -> (acc := !acc ^ r; ValNone)
    |OpSymb s -> get_assigned s assignments
    |OpString s -> ValString s
    |OpNumber n -> ValNumber n
    |OpList l -> ValList (List.map (fun i -> eval_i assignments i) l)
    |OpDict l -> (
      let dict = Hashtbl.create 100 in
      List.iter (fun (OpDictMapping (lhs, rhs)) -> Hashtbl.add dict (eval_i assignments lhs) (eval_i assignments rhs)) l;
      ValDict dict
    )
    |OpAdd (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) + (eval_i assignments rhs |> int_of_valnumber))
    |OpSub (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) - (eval_i assignments rhs |> int_of_valnumber))
    |OpMul (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) * (eval_i assignments rhs |> int_of_valnumber))
    |OpDiv (lhs, rhs) -> ValNumber ((eval_i assignments lhs |> int_of_valnumber) / (eval_i assignments rhs |> int_of_valnumber))
    |OpPow (lhs, rhs) -> ValNumber (pow (eval_i assignments lhs |> int_of_valnumber) (eval_i assignments rhs |> int_of_valnumber))
    |OpNegate u -> ValNumber(-(eval_i assignments u |> int_of_valnumber))
    |OpIfElse (comparison, ifblock, elseblock) ->
      if (truthy (eval_i assignments comparison)) then
        eval_i assignments ifblock 
      else 
        eval_i assignments elseblock
    |OpFor (init, comparison, after, block) -> (
      eval_i assignments init;
      while (truthy (eval_i assignments comparison)) do
        eval_i assignments block;
        eval_i assignments after;
      done;
      ValNone
    )
    |OpWhile (comparison, block) -> (
      while (truthy (eval_i assignments comparison)) do
        eval_i assignments block;
      done;
      ValNone
    )
    |OpStatementList sl -> (
      let rec statement_list_i = function
        |hd::tl -> (
          match eval_i assignments hd with
            |ValReturn _ as v -> v
            |_ -> statement_list_i tl
        )
        |[] -> ValNone
      in statement_list_i sl
    )
    |OpBlock ast -> eval_i ((Hashtbl.create 100)::assignments) ast
    |OpFunDef (OpSymb name, arglist, block) -> (set_assigned ~must_exist:false  name (ValFunction (arglist, block)) assignments; ValNone)
    |OpFunCall (OpSymb name, OpParamList paramlist) -> (
      match name with
        |"print" -> (
          let rec print_i = function
            |(hd::[]) -> (
              acc := (!acc) ^ (match (eval_i assignments hd) with
                |ValString s -> s
                |ValNumber n -> string_of_int n
                |ValBool b -> string_of_bool b
                |_->""
              );
              ValNone
            )
            |(hd::tl) -> (
              acc := (!acc) ^ (match (eval_i assignments hd) with
                |ValString s -> s
                |ValNumber n -> string_of_int n
                |ValBool b -> string_of_bool b
                |_->""
              ) ^ " ";
              print_i tl
            )
          in print_i paramlist
        )
        |_ -> (
          match (get_assigned name assignments) with 
            |ValFunction (OpArgList arglist, funblock) -> (
              let newScope = [Hashtbl.create 100] in
              let rec build_scope_i = function 
                |([], []) -> ()
                |([], _) -> raise (Failure ("Not enough arguments for function " ^ name))
                |(_, []) -> raise (Failure ("Too many arguments for function " ^ name))
                |((OpSymb arg)::atl, param::ptl) -> (
                  set_assigned ~must_exist:false arg (eval_i assignments param) newScope;
                  build_scope_i (atl,ptl)
                )
              in build_scope_i (arglist, paramlist);
              match eval_i (newScope@assignments) funblock with
                | ValReturn v -> v
                | _ -> ValNone
            )
            |ValNone -> raise (Failure ("No function exists called " ^ name))
            |_ -> raise (Failure ("Type is not callable"))
        )
    )
    |OpAssign (OpSymb name, rhs) -> (
      let v = eval_i assignments rhs in
      set_assigned name v assignments;
      v
    )
    |OpLetAssign (OpSymb name, rhs) -> (
      let v = eval_i assignments rhs in
      set_assigned ~must_exist:false name v assignments;
      v
    )
    |OpEquiv (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a = b)
        |(ValString a, ValString b) -> ValBool (a = b)
        |(ValBool a, ValBool b) -> ValBool (a = b)
        |_ -> ValNone
    )
    |OpNotEquiv (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a <> b)
        |(ValString a, ValString b) -> ValBool (a <> b)
        |(ValBool a, ValBool b) -> ValBool (a <> b)
        |_ -> ValNone
    )
    |OpLT (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a < b)
        |_ -> ValNone
    )
    |OpGT (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a > b)
        |_ -> ValNone
    )
    |OpLTE (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a <= b)
        |_ -> ValNone
    )
    |OpGTE (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValNumber a, ValNumber b) -> ValBool (a >= b)
        |_ -> ValNone
    )
    |OpNop -> ValNone
    |OpReturn u -> ValReturn (eval_i assignments u)
    |OpIncYield u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n+1)) assignments; ValNumber (n+1))
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpYieldInc u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n+1)) assignments; cv)
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpDecYield u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n-1)) assignments; ValNumber (n-1))
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpYieldDec u -> (
      match u with
        |OpSymb s -> (
          let cv = get_assigned s assignments in
          (match cv with
            |ValNumber n -> (set_assigned s (ValNumber (n-1)) assignments; cv)
            |_ -> ValNone
          )
        )
        |_ -> raise (Failure "Invalid type for incremenet")
    )
    |OpDot (lhs, rhs) -> (
      match (eval_i assignments lhs) with 
        |ValScope s -> (
          eval_i (s@assignments) rhs
        )
        |_ -> raise (Failure "Invalid type for dot operator")
    )
    |OpProgram u -> (
      eval_i assignments u;
      (match (get_assigned "layout" assignments) with
        | ValString s -> (
          let new_scope = [Hashtbl.create 100] in
          set_assigned ~must_exist:false "page" (ValScope assignments) new_scope;
          set_assigned ~must_exist:false "content" (ValString !acc) new_scope;
          let layout_path = "source/_layouts/"^s^".html" in
          if Sys.file_exists layout_path then (
            let ast = parsefile layout_path in
            acc := eval ~default_assignments:new_scope ~path_prefix:"source/_layouts" ast;
          )
        )
        | ValNone -> ());
      ValNone
    )
    |OpAccess (lhs, rhs) -> (
      match (eval_i assignments lhs, eval_i assignments rhs) with
        |(ValString s, ValNumber n) -> ValString (string_of_char s.[n])
        |(ValList l, ValNumber n) -> List.nth l n
        |(ValDict d, v) -> (
          match (Hashtbl.find_opt d v) with
            |Some v -> v
            |None -> ValNone
        )
        |_ -> raise (Failure "Invalid combination for access")
    )
    |OpInclude (OpString s) -> (
      let path = path_prefix ^ s in
      if Sys.file_exists path then (
        acc := (!acc) ^ (eval ~path_prefix:path_prefix (parsefile path))
      );
      ValNone
    )
    |_ -> raise (Failure "Unimplemented")
  in eval_i default_assignments ast;
  !acc
and parsefile fn =
  let ic = open_in fn in
  let length = in_channel_length ic in
  let buffer = Bytes.create length in
  really_input ic buffer 0 length;
  let program = Bytes.to_string buffer in

  program |> lex |> parse

let join ?(exclude_last=false) ?(joiner="") l =
  let rec join_i acc = function
    |hd::[] when exclude_last -> acc
    |hd::tl when acc = "" -> join_i (hd) tl
    |hd::tl -> join_i (acc^joiner^hd) tl
    |[] -> acc
  in join_i "" l

let () = 
  if Sys.file_exists "source" then (
    if not (Sys.is_directory "source") then
      raise (Failure "'source' exists and is not a directory!");
  ) else
    raise (Failure ("Please create a 'source' directory to use " ^ Sys.argv.(0)));

  if Sys.file_exists "output" then (
    if Sys.is_directory "output" then (
      let rec remove_rec = function
        | f::fs when Sys.is_directory f ->
          Sys.readdir f
            |> Array.to_list
            |> List.map (Filename.concat f)
            |> List.append fs
            |> remove_rec;
          Unix.rmdir f
        | f::fs -> (Sys.remove f; remove_rec fs)
        | [] -> ()
      in
        remove_rec ["output"]
    ) else (
      raise (Failure "'output' exists and is not a directory!")
    )
  );

  let rec gen = function
    | f::fs when (Sys.is_directory f) && ((Filename.basename f).[0] <> '_') ->
      Sys.readdir f
        |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs
        |> gen
    | f::fs when (Filename.basename f).[0] <> '_' -> (
      let path_list = String.split_on_char '/' f in
      let result = eval ~path_prefix:((join ~exclude_last:true ~joiner:"/" path_list) ^ "/") (parsefile f) in
      let (_::path) = path_list in
      let output_path = "output"::path in
      let rec create_path_if_not_exist prefix = function
        |(to_create::tail) when tail <> [] -> (
          let to_check = prefix ^ to_create in
          if Sys.file_exists to_check then (
            if Sys.is_directory to_check then 
              create_path_if_not_exist (to_check ^ "/") tail
            else
              raise (Failure ("Could not create directory " ^ to_check))
          ) else (
            Unix.mkdir to_check 0o777;
            create_path_if_not_exist (to_check ^ "/") tail
          )
        )
        |_ -> ()
      in create_path_if_not_exist "" output_path;
      let asbytes = Bytes.of_string result in
      let bytes_length = Bytes.length asbytes in
      let oc = open_out (join ~joiner:"/" output_path) in
      output oc asbytes 0 bytes_length;
      gen fs
    )
    | f::fs -> gen fs
    | [] -> ()
  in gen ["source"]