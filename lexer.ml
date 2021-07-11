 #use "Token.ml"

 let lexical_analysis source =

  let len = String.length source in

  let next_char pos =
    String.get source (pos + 1) in

  let rec comment_end pos =
    if (next_char pos) = '*' && (next_char (pos + 1)) = '/'
      then  (pos + 3)
    else comment_end (pos + 1) in

  let eat_number pos = 
    let is_number c =
      match c with
      |'0'..'9' -> true
      | _ -> false in
    let rec eat_nr start ennd =
      if ennd <= len && is_number (String.get source ennd) then
        eat_nr start (ennd + 1)
      else
       (int_of_string (String.sub source start (ennd-start)),ennd)
      in
      eat_nr pos (pos + 1) in

  let eat_word pos =
    let is_alpha_numeric c =
      match c with
      | 'a'..'z'| 'A'..'Z' | '0'..'9' | '_' -> true
      | _ -> false in
    let rec eat_wrd start ennd  =
      if ennd <= len &&  is_alpha_numeric (String.get source ennd) then
        eat_wrd start (ennd + 1)
      else
        (String.sub source start (ennd-start), ennd)
      in
      eat_wrd pos (pos + 1) in

  let rec analyse current_pos token_list =
    if current_pos = len then token_list
    else
    match String.get source current_pos with
    |'(' -> analyse (current_pos + 1) (LPAREN::token_list)
    |')' -> analyse (current_pos + 1) (RPAREN::token_list)
    |'{' -> analyse (current_pos + 1) (LCURLYBRACE::token_list)
    |'}' -> analyse (current_pos + 1) (RCURLYBRACE::token_list)
    |'[' -> analyse (current_pos + 1) (LANGLEBRACE::token_list)
    |']' -> analyse (current_pos + 1) (RANGLEBRACE::token_list)
    |',' -> analyse (current_pos + 1) (COMMA::token_list)
    |';' -> analyse (current_pos + 1) (SEMICOLON::token_list)
    |'+' -> analyse (current_pos + 1) (PLUS::token_list)
    |'-' -> analyse (current_pos + 1) (MINUS::token_list)
    |'*' -> analyse (current_pos + 1) (STAR::token_list) 
    |'/' -> 
      if (current_pos < len -1) && (next_char current_pos) = '*'
        then analyse (comment_end current_pos + 1) token_list
      else  analyse (current_pos + 1) (SLASH::token_list)
    |'<' ->
      if (current_pos < len -1) && (next_char current_pos) = '='
        then analyse (current_pos + 2) (LESSEQUAL::token_list)
      else analyse (current_pos + 1) (LESS::token_list)
    |'>' ->
      if (current_pos < len -1) && (next_char current_pos) = '='
        then analyse (current_pos + 2) (GREATEREQUAL::token_list)
      else analyse (current_pos + 1) (GREATER::token_list)
    |'=' ->
      if (current_pos < len - 1) && (next_char current_pos) = '='
        then analyse (current_pos + 2) (EQUALEQUAL::token_list)
      else analyse (current_pos + 1) (EQUAL::token_list)
    |'!' ->
      if (current_pos < len - 1) && (next_char current_pos) = '='
        then analyse (current_pos + 2) (NOTEQUAL::token_list)
        else failwith ("lexical error, unknown token")
    (* skip whitespaces *)
    |' '| '\t' | '\r'|'\n' -> analyse (current_pos + 1) token_list
    (* letter *)
    | '\'' -> analyse (current_pos + 3) (LETTER(String.get source (current_pos + 1))::token_list)
    (* number *)
    | '0'..'9' ->
     let (number, next_pos) = (eat_number current_pos) in
      analyse next_pos (NUMBER(number)::token_list)
    (* keywords and identifiers *)
    |_ ->
      let (word, next_pos) = eat_word current_pos in
      match word with
      |"else" -> analyse next_pos (ELSE::token_list)
      |"if" -> analyse next_pos (IF::token_list)
      |"int" -> analyse next_pos (INT::token_list)
      |"return" -> analyse next_pos (RETURN::token_list)
      |"void" -> analyse next_pos (VOID::token_list)
      |"while" -> analyse next_pos (WHILE::token_list)
      | _ -> analyse next_pos (IDENTIFIER(word)::token_list)
  in
  List.rev (analyse 0 [])

