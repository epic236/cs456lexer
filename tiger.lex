type pos = int
type lexresult = Tokens.token
  
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%

%%

[0-9]+ => (
    case Int.fromString yytext of
    SOME n => Tokens.INT(n ,yypos, yypos + size yytext)
    | None => ErrorMsg.error yypos "Error Integer"
);

[a-zA-Z][a-zA-Z0-9_]* => (
    case yytext of
    "type" => Tokens.TYPE(yypos, yypos + size yytext)
    | "var" => TOKENS.VAR(yypos, yypos + size yytext)
    | "function" => Tokens.FUNCTION(yypos, yypos + size yytext)
    | "break" => Tokens.BREAK(yypos, yypos + size yytext)
    | "of" => Tokens.OF(yypos, yypos + size yytext)
    | "end" => Token.END(yypos, yypos + size yytext)
    | "in" => Tokens.IN(yypos, yypos + size yytext)
    | "nil" => Tokens.NIL(yypos, yypos + size yytext)
    | "let" => Tokens.LET(yypos, yypos + size yytext)
    | "to" => Tokens.TO(yypos, yypos + size yytext)
    | "for" => Tokens.FOR(yypos, yypos + size yytext)
    | "while" => Tokens.WHILE(yypos, yypos + size yytext)
    | "else" => Tokens.ELSE(yypos, yypos + size yytext)
    | "then" => Tokens.THEN(yypos, yypos + size yytext)
    | "if" => Tokens.IF(yypos, yypos + size yytext)
    | "array" => Tokens.ARRAY(yypos, yypos + size yytext)
    | _ => Tokens.ID(yytext, yypos, yypos + size yytext)
)




"\n"	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
"\t" => (lineNum := !lineNum; linePos := yypos+3; continue());
"\"" => (Tokens.STRING(yypos,yypos+1));
"\\" => (continue());
"\ddd" => (continue());

","	=> (Tokens.COMMA(yypos,yypos+1));
":" => (Tokens.COLON(yypos,yypos+1));
";" => (Tokens.SEMICOLON(yypos,yypos+1));
"(" => (Tokens.LPAREN(yypos,yypos+1));
")" => (Tokens.RPAREN(yypos,yypos+1));
"[" => (Tokens.LBRACK(yypos,yypos+1));
"]" => (Tokens.RBRACK(yypos,yypos+1));
"{" => (Tokens.LBRACE(yypos,yypos+1));
"}" => (Tokens.RBRACE(yypos,yypos+1));
"." => (Tokens.DOT(yypos,yypos+1));
"+" => (Tokens.PLUS(yypos,yypos+1));
"-" => (Tokens.MINUS(yypos,yypos+1));
"*" => (Tokens.TIMES(yypos,yypos+1));
"/" => (Tokens.DIVIDE(yypos,yypos+1));
"=" => (Tokens.EQ(yypos,yypos+1));
"<>" => (Tokens.NEQ(yypos,yypos+2));
"<" => (Tokens.LT(yypos,yypos+1));
"<=" => (Tokens.LE(yypos,yypos+2));
">" => (Tokens.GT(yypos,yypos+1));
">=" => (Tokens.GE(yypos,yypos+2));
"&" => (Tokens.AND(yypos,yypos+1));
"|" => (Tokens.OR(yypos,yypos+1));
":=" => (Tokens.ASSIGN(yypos,yypos+2));


.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());



