open Lexer

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

  pow := <incdec> {Pow <incdec>}*

  factor := <pow> {(Mul|Div) <pow>}*

  math_expr := <factor> {(Add|Sub) <factor>}*

  bool_expr := <math_expr> Equiv <math_expr>
             | <math_expr> NotEquiv <math_expr>
             | <math_expr> LT <math_expr>
             | <math_expr> GT <math_expr>
             | <math_expr> GTE <math_expr>
             | <math_expr> LTE <math_expr>
             | <math_expr>
  
  expr := <bool_expr> ((Equals|AddEquals|SubEquals|MulEquals|DivEquals|PowEquals) <expr>)?

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

  foreach_statement := Foreach LParen <symbol> (Comma <symbol>)? in <expr> RParen <block>

  statement := function_def
             | expr_statement
             | for_loop
             | while_loop
             | if_statement
             | return_statement
             | let_statement
             | include_statement
             | foreach_statement
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
  |OpRep of ast * ast * ast
  |OpForEach of ast * ast * ast
  |OpForEachKV of ast * ast * ast * ast

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
      |t::_ -> raise (syntax_error t (TokStr ""))
      |_->raise (Failure("No More Tokens!")));
  in

  let eat_symbol () =
    tokens := (match !tokens with
      |(TokSym _)::hd -> hd
      |t::_ -> raise (syntax_error t (TokSym ""))
      |_->raise (Failure("No More Tokens!")));
  in

  let eat_number () =
    tokens := (match !tokens with
      |(TokNum _)::hd -> hd
      |t::_ -> raise (syntax_error t (TokNum 0))
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
      |(TokLBracket)::(TokRBracket)::_ -> (eat TokLBracket; eat TokRBracket; OpList [])
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
  and assign_expr lhs =
    match !tokens with
      |TokEquals::_ -> (eat TokEquals; OpAssign(lhs, expr ()))
      |TokAddEquals::_ -> (eat TokAddEquals; OpAssign(lhs, OpAdd(lhs, expr ())))
      |TokSubEquals::_ -> (eat TokSubEquals; OpAssign(lhs, OpSub(lhs, expr ())))
      |TokMulEquals::_ -> (eat TokMulEquals; OpAssign(lhs, OpMul(lhs, expr ())))
      |TokDivEquals::_ -> (eat TokDivEquals; OpAssign(lhs, OpDiv(lhs, expr ())))
      |TokPowEquals::_ -> (eat TokPowEquals; OpAssign(lhs, OpPow(lhs, expr ())))
  and expr () =
    let lhs = bool_expr() in
    match !tokens with
      |(TokEquals|TokAddEquals|TokSubEquals|TokMulEquals|TokDivEquals|TokPowEquals)::_ -> assign_expr lhs
      |_->lhs
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
      |(TokSym "let")::(TokSym s)::_ -> (
        eat_symbol ();
        eat_symbol ();
        let OpAssign (lhs, rhs) = assign_expr (OpSymb s) in
        OpLetAssign (lhs, rhs)
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Let"))
  and include_statement () =
    match !tokens with
      |(TokSym "include")::(TokStr s)::_ -> (
        eat_symbol ();
        eat_string ();
        OpInclude (OpString s)
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Include"))
  and foreach_statement () =
    match !tokens with
      |(TokSym "foreach")::(TokLParen)::(TokSym k)::(TokComma)::(TokSym v)::(TokSym "in")::_ -> (
        eat_symbol ();
        eat TokLParen;
        eat_symbol ();
        eat TokComma;
        eat_symbol ();
        eat_symbol ();
        let vv = expr () in
        eat TokRParen;
        OpForEachKV (OpSymb k, OpSymb v, vv, block ())
      )
      |(TokSym "foreach")::(TokLParen)::(TokSym s)::(TokSym "in")::_ -> (
        eat_symbol ();
        eat TokLParen;
        eat_symbol ();
        eat_symbol ();
        let v = expr () in
        eat TokRParen;
        OpForEach (OpSymb s, v, block ())
      )
      |_ -> raise (syntax_error (peek () |> unwrap) (TokSym "Foreach"))
  and statement () =
    match (peek () |> unwrap) with
      |TokSym "fn" -> function_def ()
      |TokSym "if" -> if_statement ()
      |TokSym "for" -> for_loop ()
      |TokSym "while" -> while_loop ()
      |TokSym "return" -> return_statement ()
      |TokSym "let" -> let_statement ()
      |TokSym "include" -> include_statement ()
      |TokSym "foreach" -> foreach_statement ()
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
  in 
  let result = program () in
  print_endline "Finished Parsing...";
  result