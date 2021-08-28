%option noyywrap yylineno
%{

  #include <string.h>
  #include "micko.tab.h"
  #include "defs.h"
  extern YYSTYPE yylval;

%}

%%

[ \t\n]+  { /*skip*/ }

"if"     	{ return _IF; }
"else"   	{ return _ELSE; }
"return" 	{ return _RETURN; }
"oiai"          { return _OIAI;  }
"hana"          { return _HANA; }
"jiro"          { return _JIRO; }
"tranga"        { return _TRANGA; }
"finish"        { return _FINISH; }
"toerana"       { return _TOERANA; }


"int"           { yylval.i = INT; return _TIP; }
"unsigned"      { yylval.i = UINT; return _TIP; }
"void"          { yylval.i = VOID; return _TIP;  }

"(" 		{ return _LZAGRADA; }
")" 		{ return _DZAGRADA; }
"{"   		{ return _LVITICASTA; }
"}" 		{ return _DVITICASTA; }
";"		{ return _TZAREZ; }
","             { return _ZAREZ; }
"["             { return _LUGLASTA; }
"]"             { return _DUGLASTA; }
"="		{ return _DODELA; }

"=>"            { return _IMPL;  }

"and"           { yylval.i = AND; return _LOP;  }
"or"            { yylval.i = OR; return _LOP;  }

"?"             { return _UPITNIK;  }
":"             { return _DTACKA;   }

"+"		{ yylval.i = ADD; return _AOP; }
"-"		{ yylval.i = SUB; return _AOP; }
"++"            { return _INC; }
"--"            { return _DEC; }

"<"		{ yylval.i = LT; return _ROP; }
">"		{ yylval.i = GT; return _ROP; }
"==" 		{ yylval.i = EQ; return _ROP;  }
"<="            { yylval. i = LE; return _ROP;  }
">="            { yylval.i = GE; return _ROP;  }
"!="            { yylval.i = NE; return _ROP;  }

[a-zA-Z][a-zA-Z0-9]* { yylval.s = strdup(yytext); return _ID; }		
[+-]?[0-9]{1,10}     { yylval.s = strdup(yytext); return _INT; }
[0-9]{1,10}[uU]      { yylval.s = strdup(yytext);
			yylval.s[yyleng-1] = 0;
			return _UINT; }

"//".*               {/*preskoci*/ }
"/*"(.|\n)*?"*/"      { /*preskoci*/ }

.               { printf("line %d: LEKSICKA GRESKA na char karakteru %c\n", yylineno,*yytext); }

%%

