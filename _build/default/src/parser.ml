open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  let (toks, o) = parse_OR toks in
  (*if toks <> [EOF] then 
    raise (InvalidInputException "parse_expr failed at expr")
  else *)
    (toks, o)

and parse_OR (toks : token list) : token list * expr  = 
  let (toks, a) = parse_AND toks in
  if lookahead toks = Tok_Or then
    let toks = match_token toks Tok_Or in
    let (toks, o) = parse_OR toks in
    (toks, Or(a,o))
  else (toks, a)

and parse_AND toks = 
  let (toks, eqe) = parse_EQE toks in
  if lookahead toks = Tok_And then 
    let toks = match_token toks Tok_And in
    let (toks, a) = parse_AND toks in 
    (toks, And(eqe, a))
  else (toks, eqe)
  
and parse_EQE toks = 
  let (toks, rele) = parse_RELE toks in
  if lookahead toks = Tok_Equal then 
    let toks = match_token toks Tok_Equal in 
    let (toks, eqe) = parse_EQE toks in
    (toks, Equal(rele, eqe))
  else if lookahead toks = Tok_NotEqual then
    let toks = match_token toks Tok_NotEqual in 
    let (toks, eqe) = parse_EQE toks in
    (toks, NotEqual(rele, eqe))
  else (toks, rele)

and parse_RELE toks =
  let (toks, adde) = parse_ADDE toks in
  if lookahead toks = Tok_Less then
    let toks = match_token toks Tok_Less in
    let (toks, rele) = parse_RELE toks in
    (toks, Less(adde, rele))
  else if lookahead toks = Tok_Greater then
    let toks = match_token toks Tok_Greater in
    let (toks, rele) = parse_RELE toks in
    (toks, Greater(adde, rele))
  else if lookahead toks = Tok_LessEqual then
    let toks = match_token toks Tok_LessEqual in
    let (toks, rele) = parse_RELE toks in
    (toks, LessEqual(adde, rele))
  else if lookahead toks = Tok_GreaterEqual then
    let toks = match_token toks Tok_GreaterEqual in
    let (toks, rele) = parse_RELE toks in
    (toks, GreaterEqual(adde, rele))
  else (toks, adde)

and parse_ADDE toks =
  let (toks, multe) = parse_MULTE toks in
  match lookahead toks with 
  | Tok_Add -> 
    let toks = match_token toks Tok_Add in
    let (toks, adde) = parse_ADDE toks in
    (toks, Add(multe, adde))
  | Tok_Sub ->
    let toks = match_token toks Tok_Sub in 
    let (toks, adde) = parse_ADDE toks in 
    (toks, Sub(multe, adde))
  | _ -> (toks, multe)
  (*if lookahead toks = Tok_Add then
    let toks = match_token toks Tok_Add in
    let (toks, adde) = parse_ADDE toks in
    (toks, Add(multe, adde))
  else if lookahead toks = Tok_Sub then 
    let toks = match_token toks Tok_Sub in 
    let (toks, adde) = parse_ADDE toks in 
    (toks, Sub(multe, adde))
  else (toks, multe)*)

and parse_MULTE toks = 
  let (toks, pow) = parse_POW toks in 
  if lookahead toks = Tok_Mult then 
    let toks = match_token toks Tok_Mult in 
    let (toks, multe) = parse_MULTE toks in 
    (toks, Mult(pow, multe))
  else if lookahead toks = Tok_Div then
    let toks = match_token toks Tok_Div in 
    let (toks, multe) = parse_MULTE toks in 
    (toks, Div(pow, multe))
  else (toks, pow)

and parse_POW toks = 
  let (toks, un) = parse_UN toks in 
  if lookahead toks = Tok_Pow then 
    let toks = match_token toks Tok_Pow in 
    let (toks, pow) = parse_POW toks in
    (toks, Pow(un, pow))
  else (toks, un)

and parse_UN toks = 
  if lookahead toks = Tok_Not then 
    let toks = match_token toks Tok_Not in
    let (toks, un) = parse_UN toks in 
    (toks, Not(un))
  else 
    let (toks, pri) = parse_PRI toks in
    (toks, pri)

and parse_PRI toks = 
  match lookahead toks with 
  | Tok_Int i -> 
    let toks = match_token toks (Tok_Int i) in
    (toks, Int i) 
  | Tok_Bool var -> 
    let toks = match_token toks (Tok_Bool var) in    
    (toks, Bool var)
  | Tok_ID var ->
    let toks = match_token toks (Tok_ID var) in 
    (toks, ID var)
  | Tok_LParen -> 
    let toks = match_token toks Tok_LParen in 
    let (toks, o) = parse_OR toks in
    let toks = match_token toks Tok_RParen in 
    (toks, o) 
  | _ -> raise (InvalidInputException "parse_expr failed at PRI")


let rec parse_stmt toks : stmt_result =
  if lookahead toks = Tok_RBrace then (toks, NoOp)
  else
    let (toks, so) = parse_SO toks in
    match lookahead toks with 
    | Tok_Int_Type -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | Tok_Bool_Type -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | Tok_ID id -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | Tok_Print -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | Tok_If -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | Tok_For -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | Tok_While -> 
      let (toks, s) = parse_stmt toks in 
      (toks, Seq(so, s))
    | _ -> (toks, Seq(so, NoOp))
  
and parse_SO toks = 
  match lookahead toks with 
  | Tok_Int_Type -> parse_DS toks
  | Tok_Bool_Type -> parse_DS toks
  | Tok_ID id -> parse_AS toks
  | Tok_Print -> parse_PS toks
  | Tok_If -> parse_IS toks
  | Tok_For -> parse_FS toks
  | Tok_While -> parse_WS toks
  | _ -> raise (InvalidInputException "parse_stmt failed at SO")

and parse_DS toks = 
  match lookahead toks with 
  | Tok_Int_Type -> 
    (let toks = match_token toks Tok_Int_Type in
    match lookahead toks with 
    | Tok_ID id -> 
      let toks = match_token toks (Tok_ID id) in
      let semi = lookahead toks in 
      let toks = match_token toks (Tok_Semi) in
      (toks, Declare(Int_Type, id))
    | _ -> raise (InvalidInputException "parse_stmt failed at DS"))
  | Tok_Bool_Type ->
    (let toks = match_token toks Tok_Bool_Type in
    match lookahead toks with 
    | Tok_ID id -> 
      let toks = match_token toks (Tok_ID id) in
      let semi = lookahead toks in 
      let toks = match_token toks (Tok_Semi) in
      (toks, Declare(Bool_Type, id))
    | _ -> raise (InvalidInputException "parse_stmt failed at DS"))
  | _ -> raise (InvalidInputException "parse_stmt failed at DS")

and parse_AS toks =
  match lookahead toks with 
  | Tok_ID id -> 
    let toks = match_token toks (Tok_ID id) in
    let toks = match_token toks Tok_Assign in
    let (toks, result) = parse_expr toks in
    let toks = match_token toks Tok_Semi in 
    (toks, Assign(id, result)) (*)
  | _ -> raise (InvalidInputException "parse_stmt failed at AS") *)


 (* - PrintStmt -> `printf` `(` Expr `)` `;` *)
and parse_PS toks = 
  let toks = match_token toks Tok_Print in 
  let toks = match_token toks Tok_LParen in 
  let (toks, result) = parse_expr toks in 
  let toks = match_token toks Tok_RParen in 
  let toks = match_token toks Tok_Semi in 
  (toks, Print(result))

and parse_IS toks = 
  let toks = match_token toks Tok_If in
  let toks = match_token toks Tok_LParen in 
  let (toks, expr_result) = parse_expr toks in 
  let toks = match_token toks Tok_RParen in 
  let toks = match_token toks Tok_LBrace in 
  let (toks, stmt1_result) = parse_stmt toks in 
  let toks = match_token toks Tok_RBrace in
  let (toks, stmt2_result) = 
    match lookahead toks with 
    | Tok_Else -> 
      let toks = match_token toks Tok_Else in
      let toks = match_token toks Tok_LBrace in 
      let (toks, stmt_result) = parse_stmt toks in 
      let toks = match_token toks Tok_RBrace in 
      (toks, stmt_result)
    | _ -> (toks, NoOp) 
  in 
  (toks, If(expr_result, stmt1_result, stmt2_result))
  
  (*ForStmt -> `for` `(` ID `from` Expr `to` Expr `)` `{` Stmt `}`*)

and parse_FS toks = 
    let toks = match_token toks Tok_For in 
    let toks = match_token toks Tok_LParen in 
    match lookahead toks with 
    | Tok_ID id -> 
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_From in 
      let (toks, expr1) = parse_expr toks in 
      let toks = match_token toks Tok_To in 
      let (toks, expr2) = parse_expr toks in 
      let toks = match_token toks Tok_RParen in 
      let toks = match_token toks Tok_LBrace in 
      let (toks, stmt) = parse_stmt toks in 
      let toks = match_token toks Tok_RBrace in 
      (toks, For(id, expr1, expr2, stmt)) (*)
    | _ -> raise (InvalidInputException "parse_stmt failed at FS") *)


   (* WhileStmt -> `while` `(` Expr `)` `{` Stmt `}`*)
and parse_WS toks = 
    let toks = match_token toks Tok_While in 
    let toks = match_token toks Tok_LParen in 
    let (toks, expr) = parse_expr toks in 
    let toks = match_token toks Tok_RParen in 
    let toks = match_token toks Tok_LBrace in 
    let (toks, stmt) = parse_stmt toks in 
    let toks = match_token toks Tok_RBrace in 
    (toks, While(expr, stmt))


(*Main -> `int` `main` `(` `)` `{` Stmt `}` `EOF` *)

let parse_main toks : stmt =
  let toks = match_token toks Tok_Int_Type in 
  let toks = match_token toks Tok_Main in
  let toks = match_token toks Tok_LParen in
  let toks = match_token toks Tok_RParen in
  let toks = match_token toks Tok_LBrace in
  let (toks, stmt) = parse_stmt toks in
  let toks = match_token toks Tok_RBrace in
  let toks = match_token toks EOF in
  stmt
  
