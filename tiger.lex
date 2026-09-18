type pos = int
type lexresult = Tokens.token
  
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1


val StringBuffer: string ref = ref ""
val StringIndex: int ref = ref 0
val StringState: int ref = ref 0

val CommentCount: int ref = ref 0


fun eof() =
    let
        val pos = hd(!linePos)  
    in
        if !CommentCount > 0 then
            ErrorMsg.error pos "Error: unclosed comment"
        else if !StringState = 1 then
            ErrorMsg.error pos "Error: unclosed string"
        else ();
        Tokens.EOF(pos, pos)
    end


fun asciiCode s = str(chr(valOf(Int.fromString(String.extract(s, 1, NONE)))))


%%
%s COMMENT STRING SPACE;
%%

<INITIAL> [\ \t\r] => (continue());
<INITIAL> \n => (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());

<INITIAL> "/*" => (CommentCount := 1; YYBEGIN COMMENT; continue());

<COMMENT> "/*" => (CommentCount := !CommentCount + 1; continue());
<COMMENT> "*/" => (CommentCount := !CommentCount - 1;
    if !CommentCount = 0
    then YYBEGIN INITIAL
    else ();
    continue()
);

<COMMENT> "\n" => (
    lineNum := !lineNum + 1;
    linePos := yypos :: !linePos;
    continue()
);

<COMMENT> [^\n] => (continue());


<INITIAL> [0-9]+ => (
    case Int.fromString yytext of SOME n => Tokens.INT(n ,yypos, yypos + size yytext)
    | NONE => (ErrorMsg.error yypos "Error: illegal integer"; Tokens.INT(0 ,yypos, yypos + size yytext))
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
<STRING> [^"\\\n]* => (StringBuffer := !StringBuffer ^ yytext; continue());
<STRING> \\n => (StringBuffer := !StringBuffer ^ "\n"; continue());
<STRING> \\t => (StringBuffer := !StringBuffer ^ "\t"; continue());
<STRING> \\\" => (StringBuffer := !StringBuffer ^ "\""; continue());
<STRING> \\\\ => (StringBuffer := !StringBuffer ^ "\\"; continue());
<STRING> \\[0-9][0-9][0-9] => (StringBuffer := !StringBuffer ^ asciiCode(yytext); continue());
<STRING> \" => (StringState := 0; YYBEGIN INITIAL; Tokens.STRING(!StringBuffer, !StringIndex, yypos));
<STRING> "\\" => (YYBEGIN SPACE; continue());

<SPACE> [ \t\r\n] => (continue());
<SPACE> "\\" => (YYBEGIN STRING; continue());

<STRING> \n => (ErrorMsg.error yypos ("Error: illegal character " ^ yytext); continue());
<STRING> . => (ErrorMsg.error yypos ("Error: illegal character " ^ yytext); continue());
<INITIAL> # => (ErrorMsg.error yypos ("Error: illegal character " ^ yytext); continue());