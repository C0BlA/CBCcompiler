%{
#include <string.h>
#include "cbc.h"
#define MAXSYM	100
#define	MAXSYMLEN	20
char symtbl[MAXSYM][MAXSYMLEN];
int yylex(void);
void yyerror(const char *s);
typedef struct nodeType {
	int token;
	int tokenval;
	struct nodeType *son;
	struct nodeType *brother;
	} Node;

Node* MakeNode(int, ...);


int lineno=1;
int insertsym();
int maxsym=0;
char s[MAXSYMLEN];
int temp;

%}
sp		[ \t]
ws		{sp}+
nl		\n
eletter		[A-Za-z]
hletter		[\xb0-\xfe][\xa0-\xfe]
letter		({eletter}|{hletter})
digit		[0-9]
id		{letter}({letter}|{digit})*




%%
{ws}		{/* do nothing */}
{nl}		{lineno++; }


\(		{return('('); }
\)		{return(')'); }
\{		{return('{'); }
\}		{return('}'); }




"//".*	{/*	한줄 주석*/}
"/*"([^*]|\*+[^*/])*\*+"/"	{/*여러줄 주석*/}

\=		{return(ASSGN); }



if		{return(IF);}
else	{return(ELSE);}

while	{return(WHILE);}



\+		{return(ADD); }
\-		{return(SUB); }
\*		{return(MUL);}
\/		{return(DIV);}
\%		{return(MOD);}


"++" 	{return(INCREASE);}
"--"	{return(DECREASE);}


">>"	{return(RSHIFT);}
"<<"	{return(LSHIFT);}


">="	{return(GREATEREQUAL);}
"<="	{return(LESSEQUAL);}
"=="	{return(EQUAL);}
"!="	{return(NOTEQUAL);}


\>		{return(GREATER);}
\<		{return(LESS);}


"&&"		{return(AND);}
"||"		{return(OR);}
\!			{return(NOT);}


\&			{return(BITAND);}
\|			{return(BITOR);}
\^			{return(BITXOR);}
\~			{return(BITNOT);}


return	{return(RETURN);}







;		{return(STMTEND); }
main		{return(MAIN); }
{id}    {
    temp = insertsym(yytext);
    printf("토큰 인식: %s (index %d)\n", yytext, temp);  // debug
    yylval = MakeNode(ID, temp);
    return(ID);
}
-?{digit}+		{sscanf(yytext, "%d", &temp); yylval=MakeNode(NUM, temp); return(NUM);}
.		{printf("invalid token %s\n", yytext); }
%%

int insertsym(s)
char *s;
{
int i;

	for(i=0;i<maxsym; i++) {
		if(strcmp(s, symtbl[i])==0) return(i);
		}
	if (i<MAXSYM-1) {
		strcpy(symtbl[maxsym],s);
		maxsym++;
		return(maxsym-1);
		}
	else
		{printf("symbol table overflow\n");}
	return(0);
}

int yywrap(void) { return 1; }
