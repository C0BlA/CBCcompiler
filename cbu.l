%{
#include <string.h>
#include "cbc.h"
#define MAXSYM	100
#define	MAXSYMLEN	20
char symtbl[MAXSYM][MAXSYMLEN];
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



/*추가된 내용*/

/*주석*/
"//".*	{/*	한줄 주석*/}
\/\*([^*]|\*+[^*/])*\*+\/	{/*여러줄 주석*/}

/*할당*/
\=		{return(ASSGN); }



/*분기*/
if		{return(IF);}
else	{return(ELSE);}

/*반복*/
for		{return(FOR);}
while	{return(WHILE);}

/*연산자*/

/*-----대입 연산자-----*/
"+=" 	{return(ASSGNADD);}
"-=" 	{return(ASSGNSUB);}
"*="	{return(ASSGNMUL);}
"/="	{return(ASSGNDIV);}
"%="	{return(ASSGNMOD);}


/*-----산술 연산자-----*/
\+		{return(ADD); }
\-		{return(SUB); }
\*		{return(MUL);}
\/		{return(DIV);}
/%		{return(MOD);}


/*-----증감 연산자-----*/
"++" 	{return(INCREASE);}
"--"	{return(DECREASE);}


/*-----비트 연산자_1-----*/
">>"	{return(RSHIFT);}
"<<"	{return(LSHIFT);}


/*-----관계 연산자-----*/
">="	{return(GREATEREQUAL);}
"<="	{return(LESSEQUAL);}
"=="	{return(EQUAL);}
"!="	{return(NOTEQUAL);}

\>		{return(GREATER);}
\<		{return(LESS);}


/*-----논리 연산자-----*/
"&&"		{return(AND);}
"||"		{return(OR);}
\!			{return(NOT);}


/*-----비트 연산자_2-----*/
\&			{return(BITAND);}
\|			{return(BITOR);}
\^			{return(BITXOR);}
\~			{return(BITNOT);}


/*반환*/
return	{return(RETURN);}
perror	{return(PERROR);}







;		{return(STMTEND); }
main		{return(MAIN); }
{id}		{temp=insertsym(yytext); yylval=MakeNode(ID, temp); return(ID);}
\*{id}		{temp=insertsym(yytext+1); yylval=MakeNode(POINTERID, temp); return(POINTERID);}
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

yywrap() {}