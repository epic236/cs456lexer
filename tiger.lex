type pos = int
type lexresult = Tokens.token
  
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun asciiCode s = str(chr(valOf(Int.fromString(String.extract(s, 1, NONE)))))


val StringBuffer: string ref = ref ""
val StringIndex = ref 0
val StringState = ref 0

%%
%s COMMENT STRING;
%%

<INITIAL> [\ \t\n\r] => (continue());


<INITIAL> "/*" => (YYBEGIN COMMENT; continue());
<COMMENT> "*/" => (YYBEGIN INITIAL; continue());
<COMMENT> . => (continue());
<COMMENT> "\n" => (continue());


<INITIAL> [0-9]+ => (
    case Int.fromString yytext of SOME n => Tokens.INT(n ,yypos, yypos + size yytext)
    | NONE => (ErrorMsg.error yypos "Error Integer"; Tokens.INT(0 ,yypos, yypos + size yytext))
);


<INITIAL>"type" => (Tokens.TYPE(yypos, yypos + size yytext));
<INITIAL>"var" => (Tokens.VAR(yypos, yypos + size yytext));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos + size yytext));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL>"of" => (Tokens.OF(yypos, yypos + size yytext));
<INITIAL>"end" => (Tokens.END(yypos, yypos + size yytext));
<INITIAL>"in" => (Tokens.IN(yypos, yypos + size yytext));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos + size yytext));
<INITIAL>"let" => (Tokens.LET(yypos, yypos + size yytext));
<INITIAL>"to" => (Tokens.TO(yypos, yypos + size yytext));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos + size yytext));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>"if" => (Tokens.IF(yypos, yypos + size yytext));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos + size yytext));

<INITIAL> [a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext, yypos, yypos + size yytext));



<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":" => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"(" => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"[" => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{" => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"." => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+" => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"=" => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL>">" => (Tokens.GT(yypos,yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL>"&" => (Tokens.AND(yypos,yypos+1));
<INITIAL>"|" => (Tokens.OR(yypos,yypos+1));
<INITIAL>":=" => (Tokens.ASSIGN(yypos,yypos+2));



<INITIAL> \" => (StringState := 1; YYBEGIN STRING; StringIndex := yypos; StringBuffer := ""; continue());
<STRING> [ _!#-\[\]-~]* => (StringBuffer := !StringBuffer ^ yytext; continue());
<STRING> [^"\\\n]* => (StringBuffer := !StringBuffer ^ yytext; continue());
<STRING> \\n => (StringBuffer := !StringBuffer ^ "\n"; continue());
<STRING> \\t => (StringBuffer := !StringBuffer ^ "\t"; continue());
<STRING> \\\" => (StringBuffer := !StringBuffer ^ "\""; continue());
<STRING> \\\\ => (StringBuffer := !StringBuffer ^ "\\"; continue());
<STRING> \\[0-9][0-9][0-9] => (StringBuffer := !StringBuffer ^ asciiCode(yytext); continue());
<STRING> \" => (StringState := 0; YYBEGIN INITIAL; Tokens.STRING(!StringBuffer, !StringIndex, yypos));

<STRING> \n => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<STRING> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
