type token = 
  |LETTER of char
  |NUMBER of int
  |IDENTIFIER of string

  (* keywords *)
  |ELSE
  |IF
  |INT
  |RETURN
  |VOID
  |WHILE

  (* special symbols *)
  |PLUS
  |MINUS
  |STAR
  |SLASH
  |LESS
  |LESSEQUAL
  |GREATER
  |GREATEREQUAL
  |EQUALEQUAL
  |EQUAL
  |NOTEQUAL
  |SEMICOLON
  |COMMA
  |LPAREN
  |RPAREN
  |LANGLEBRACE
  |RANGLEBRACE
  |LCURLYBRACE
  |RCURLYBRACE
  (* 
  |LCOMMENT
  |RCOMMENT
  *)
