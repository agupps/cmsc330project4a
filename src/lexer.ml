open TokenTypes


let re_LParen = Str.regexp "(";;
let re_RParen = Str.regexp ")";;
let re_LBrace = Str.regexp "{";;
let re_RBrace = Str.regexp "}";;
let re_Equal = Str.regexp "==";;
let re_NotEqual = Str.regexp "!=";;
let re_Assign = Str.regexp "=";;
let re_Greater = Str.regexp ">";;
let re_Less = Str.regexp "<";;
let re_GreaterEqual = Str.regexp ">=";;
let re_LessEqual = Str.regexp "<=";;
let re_Or = Str.regexp "||";;
let re_And = Str.regexp "&&";;
let re_Not = Str.regexp "!";;
let re_Semi = Str.regexp ";";;
let re_Int_Type = Str.regexp "int";;
let re_Bool_Type = Str.regexp "bool";;
let re_Print = Str.regexp "printf";;
let re_Main = Str.regexp "main";;
let re_If = Str.regexp "if";;
let re_Else = Str.regexp "else";;
let re_For = Str.regexp "for";;
let re_From = Str.regexp "from";;
let re_To = Str.regexp "to";;
let re_While = Str.regexp "while";;
let re_Add = Str.regexp "\\+";;
let re_Sub = Str.regexp "-";;
let re_Mult = Str.regexp "\\*";;
let re_Div = Str.regexp "/";;
let re_Pow = Str.regexp "\\^";;
let re_Bool = Str.regexp "true\\|false";;
let re_Int = Str.regexp "-?[0-9]+" ;;
let re_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_space = Str.regexp "[ \n\r\t]";;

let tokenize input =
  let rec tok pos s = 
    if pos >= String.length s then [EOF]
    else if (Str.string_match re_space s pos) then 
      let token = Str.matched_string s in tok (pos+(String.length token)) s
    (*else if (Str.string_match re_Bool s pos) then 
      let token = Str.matched_string s in 
      if token = "true" then (Tok_Bool true)::tok (pos+4) s
      else (Tok_Bool false)::tok (pos+5) s *)
    else if (Str.string_match re_Int s pos) then 
      let token = Str.matched_string s in (Tok_Int (int_of_string token))::tok (pos+(String.length token)) s
    else if (Str.string_match re_ID s pos) then 
      let token = Str.matched_string s in 
        if token = "true" then (Tok_Bool true):: tok (pos+4) s
        else if token = "false" then (Tok_Bool false)::tok (pos+5) s
        else if token = "int" then Tok_Int_Type::(tok (pos+3) s)
        else if token = "bool" then Tok_Bool_Type::(tok (pos+4) s)
        else if token = "printf" then Tok_Print::(tok (pos+6) s)
        else if token = "main" then  Tok_Main::(tok (pos+4) s)
        else if token = "if" then  Tok_If::(tok (pos+2) s)
        else if token = "else" then Tok_Else::(tok (pos+4) s)
        else if token = "for" then Tok_For::(tok (pos+3) s)
        else if token = "from" then Tok_From::(tok (pos+4) s)
        else if token = "to" then Tok_To::(tok (pos+2) s)
        else if token = "while" then Tok_While::(tok (pos+5) s)
        else (Tok_ID token)::tok (pos+(String.length token)) s
    else if (Str.string_match re_LParen s pos) then Tok_LParen::(tok (pos+1) s)
    else if (Str.string_match re_RParen s pos) then Tok_RParen::(tok (pos+1) s)
    else if (Str.string_match re_LBrace s pos) then Tok_LBrace::(tok (pos+1) s)
    else if (Str.string_match re_RBrace s pos) then Tok_RBrace::(tok (pos+1) s)
    else if (Str.string_match re_Equal s pos) then Tok_Equal::(tok (pos+2) s)
    else if (Str.string_match re_NotEqual s pos) then Tok_NotEqual::(tok (pos+2) s)
    else if (Str.string_match re_Assign s pos) then Tok_Assign::(tok (pos+1) s)
    else if (Str.string_match re_GreaterEqual s pos) then Tok_GreaterEqual::(tok (pos+2) s)
    else if (Str.string_match re_LessEqual s pos) then Tok_LessEqual::(tok (pos+2) s)
    else if (Str.string_match re_Greater s pos) then Tok_Greater::(tok (pos+1) s)
    else if (Str.string_match re_Less s pos) then Tok_Less::(tok (pos+1) s)
    else if (Str.string_match re_Or s pos) then Tok_Or::(tok (pos+2) s)
    else if (Str.string_match re_And s pos) then Tok_And::(tok (pos+2) s)
    else if (Str.string_match re_Not s pos) then Tok_Not::(tok (pos+1) s)
    else if (Str.string_match re_Semi s pos) then Tok_Semi::(tok (pos+1) s)
    (*else if (Str.string_match re_Int_Type s pos) then Tok_Int_Type::(tok (pos+3) s)
    else if (Str.string_match re_Bool_Type s pos) then Tok_Bool_Type::(tok (pos+4) s)
    else if (Str.string_match re_Print s pos) then Tok_Print::(tok (pos+6) s)
    else if (Str.string_match re_Main s pos) then Tok_Main::(tok (pos+4) s)
    else if (Str.string_match re_If s pos) then Tok_If::(tok (pos+2) s)
    else if (Str.string_match re_Else s pos) then Tok_Else::(tok (pos+4) s)
    else if (Str.string_match re_For s pos) then Tok_For::(tok (pos+3) s)
    else if (Str.string_match re_From s pos) then Tok_From::(tok (pos+4) s)
    else if (Str.string_match re_To s pos) then Tok_To::(tok (pos+2) s)
    else if (Str.string_match re_While s pos) then Tok_While::(tok (pos+5) s) *)
    else if (Str.string_match re_Add s pos) then Tok_Add::(tok (pos+1) s)
    else if (Str.string_match re_Sub s pos) then Tok_Sub::(tok (pos+1) s)
    else if (Str.string_match re_Mult s pos) then Tok_Mult::(tok (pos+1) s)
    else if (Str.string_match re_Div s pos) then Tok_Div::(tok (pos+1) s)
    else if (Str.string_match re_Pow s pos) then Tok_Pow::(tok (pos+1) s)
    else raise (InvalidInputException "lexer failed")
  in tok 0 input
