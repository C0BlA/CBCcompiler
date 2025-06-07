%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define DEBUG	0

#define	 MAXSYM	100
#define	 MAXSYMLEN	20
#define	 MAXTSYMLEN	15
#define	 MAXTSYMBOL	MAXSYM/2

#define STMTLIST 500

typedef struct nodeType {
	int token;
	int tokenval;
	struct nodeType *son;
	struct nodeType *brother;
	} Node;

#define YYSTYPE Node*
	
int tsymbolcnt=0;
int errorcnt=0;

FILE *yyin;
FILE *fp;

extern char symtbl[MAXSYM][MAXSYMLEN];
extern int maxsym;
extern int lineno;

void DFSTree(Node*);
Node * MakeOPTree(int, Node*, Node*);
Node * MakeNode(int, int);
Node * MakeListTree(Node*, Node*);
void codegen(Node* );
void prtcode(int, int);

void	dwgen();
int	gentemp();
void	assgnstmt(int, int);
void	numassgn(int, int);
void	addstmt(int, int, int);
void	substmt(int, int, int);
int	insertsym(char *);
%}
/*--- 우선순위 ---*/


%right ASSGN ASSGNADD ASSGNSUB ASSGNMUL ASSGNDIV ASSGNMOD
%left OR
%left AND
%nonassoc EQUAL NOTEQUAL
%nonassoc GREATER GREATEREQUAL LESS LESSEQUAL
%left BITOR
%left BITXOR
%left BITAND
%left LSHIFT RSHIFT
%left ADD SUB
%left MUL DIV MOD
%right INCREASE DECREASE BITNOT NOT

%token	MAIN ASSGN ID NUM STMTEND START END ID2 
%token 	ADD SUB MUL DIV MOD
%token	INT FLOAT DOUBLE CHAR STRING
%token  IF ELSE WHILE
%token  GREATER GREATEREQUAL LESS LESSEQUAL EQUAL NOTEQUAL AND OR
%token	ASSGNADD ASSGNSUB ASSGNMUL ASSGNDIV ASSGNMOD
%token	RETURN 
%token	INCREASE DECREASE
%token	BITAND BITOR BITXOR BITNOT
%token	RSHIFT LSHIFT


/*--- 생성 규칙 부분 ---*/

%%
program: 	functions 		{ if (errorcnt==0) {codegen($1); dwgen();} }
		;

functions:	functions function
		| function
		;

function:		fun_header fun_body	{$$ = $2; }
		;

fun_header:	MAIN '(' ')' 
		;

fun_body:	'{' dcl_list stmt_list '}'	{$$ = $3; }
		;

dcl_list: 		dcl_list dcl
		| 		dcl
		;

stmt_list: 		stmt_list stmt 		{$$=MakeListTree($1, $2);}
		| 		stmt			{$$=MakeListTree(NULL, $1);}
		| 		error STMTEND		{ errorcnt++; yyerrok;}
		;

stmt: 		expr_stmt
		|	if_stmt
		|	while_stmt
		|	'{' stmt_list '}'
		|	RETURN expr STMTEND	{$$ = MakeOPTree(RETURN, $2, NULL)}
		;
		
expr_stmt:	expr
		|	ID ASSGN expr STMTEND	{ $1->token = ID2; $$=MakeOPTree(ASSGN, $1, $3);}
		
		/*-----복합 대입연산자-----*/
		|	expr ASSGNADD expr STMTEND { $$=MakeOPTree(ASSGNADD, $1, $3);}
		|	expr ASSGNSUB expr STMTEND { $$=MakeOPTree(ASSGNSUB, $1, $3);}
		|	expr ASSGNMUL expr STMTEND { $$=MakeOPTree(ASSGNMUL, $1, $3);}
		|	expr ASSGNDIV expr STMTEND { $$=MakeOPTree(ASSGNDIV, $1, $3);}
		|	expr ASSGNMOD expr STMTEND { $$=MakeOPTree(ASSGNMOD, $1, $3);}

		;


/*-- 산술 expr 완료, ID, 숫자는 term으로 넘김 --*/
expr: 		'(' expr')' {$$ = $2;}	
		/*-----산술 연산자-----*/
		|	expr ADD term	{ $$=MakeOPTree(ADD, $1, $3); }
		|	expr SUB term	{ $$=MakeOPTree(SUB, $1, $3); }
		|	expr MUL term 	{ $$=MakeOPTree(MUL, $1, $3); }
		|	expr DIV term 	{ $$=MakeOPTree(DIV, $1, $3); }
		|	expr MOD term 	{ $$=MakeOPTree(MOD, $1, $3); }		

		/*-----비트 연산자-----*/
		|	expr BITAND expr	{ $$=MakeOPTree(BITAND, $1, $3); }
		|	expr BITOR expr	{ $$=MakeOPTree(BITOR, $1, $3); }
		|	expr BITXOR expr	{ $$=MakeOPTree(BITXOR, $1, $3); }
		|	BITNOT expr	{ $$=MakeOPTree(BITNOT, $2, NULL); }

		|	expr RSHIFT expr	{ $$=MakeOPTree(RSHIFT, $1, $3); }
		|	expr LSHIFT expr	{ $$=MakeOPTree(LSHIFT, $1, $3); }

		/*-----증감 연산자-----*/
		|	INCREASE expr	{ $$=MakeOPTree(INCREASE, $2, NULL); }
		|	DECREASE expr	{ $$=MakeOPTree(DECREASE, $2, NULL); }
		|	expr INCREASE	{ $$=MakeOPTree(INCREASE, $1, NULL); }
		|	expr DECREASE	{ $$=MakeOPTree(DECREASE, $1, NULL); }

		/*-----관계 연산자-----*/
		|	expr GREATER term 	{ $$=MakeOPTree(GREATER, $1, $3); }		
		|	expr LESS term 	{ $$=MakeOPTree(LESS, $1, $3); }	

		|	expr GREATEREQUAL term 	{ $$=MakeOPTree(GREATEREQUAL, $1, $3); }		
		|	expr LESSEQUAL term 	{ $$=MakeOPTree(LESSEQUAL, $1, $3); }		
		|	expr EQUAL term 	{ $$=MakeOPTree(EQUAL, $1, $3); }		
		|	expr NOTEQUAL term 	{ $$=MakeOPTree(NOTEQUAL, $1, $3); }		

		/*-----논리 연산자-----*/
		|	expr AND expr 	{ $$=MakeOPTree(AND, $1, $3); }		
		|	expr OR expr 	{ $$=MakeOPTree(OR, $1, $3); }		
		|	NOT expr 				{ $$=MakeOPTree(NOT,$2, NULL);}		


		|	term
		;

/*-- term 확인 완료 --*/
term:		ID		{ $$ = $1; }
		|	NUM		{ $$ = $1;}
		;

type:
      INT
    | FLOAT
    | DOUBLE
    | CHAR
;


dcl: type ID STMTEND { insertsym(symtbl[$2->tokenval]); }
;




if_stmt :	IF '(' expr ')' stmt {$$ =MakeOPTree(IF, $3, $5); }
        |	IF '(' expr ')' stmt ELSE stmt {$$=MakeOPTree(ELSE, MakeOPTree(IF, $3, $5), $7); };

while_stmt:	WHILE '(' expr ')' stmt { $$ = MakeOPTree(WHILE, $3, $5); }



%%



int main(int argc, char *argv[]) 
{
	printf("\nsample CBU C compiler v3.0\n");
	printf("(C) Copyright by Jae Sung Lee (jasonlee@cbnu.ac.kr), 2025.\n");
	
	if (argc == 2)
		yyin = fopen(argv[1], "r");
	else {
		printf("Usage: cbc3 inputfile\noutput file is 'a.asm'\n");
		return(0);
		}
		
	fp=fopen("a.asm", "w");
	
	yyparse();
	
	fclose(yyin);
	fclose(fp);

	if (errorcnt==0) 
		{ printf("Successfully compiled. Assembly code is in 'a.asm'.\n");}
}

yyerror(s)
char *s;
{
	printf("%s (line %d)\n", s, lineno);
}


Node * MakeOPTree(int op, Node* operand1, Node* operand2)
{
Node * newnode;

	newnode = (Node *)malloc(sizeof (Node));
	newnode->token = op;
	newnode->tokenval = op;
	newnode->son = operand1;
	newnode->brother = NULL;
	operand1->brother = operand2;
	return newnode;
}

Node * MakeNode(int token, int operand)
{
Node * newnode;

	newnode = (Node *) malloc(sizeof (Node));
	newnode->token = token;
	newnode->tokenval = operand; 
	newnode->son = newnode->brother = NULL;
	return newnode;
}

Node * MakeListTree(Node* operand1, Node* operand2)
{
Node * newnode;
Node * node;

	if (operand1 == NULL){
		newnode = (Node *)malloc(sizeof (Node));
		newnode->token = newnode-> tokenval = STMTLIST;
		newnode->son = operand2;
		newnode->brother = NULL;
		return newnode;
		}
	else {
		node = operand1->son;
		while (node->brother != NULL) node = node->brother;
		node->brother = operand2;
		return operand1;
		}
}

void codegen(Node * root)
{
	DFSTree(root);
}

void DFSTree(Node * n)
{
	if (n==NULL) return;
	DFSTree(n->son);
	prtcode(n->token, n->tokenval);
	DFSTree(n->brother);
	
}

void prtcode(int token, int val)
{
    switch(token)
    {
        //  리터럴/식별자 처리
        case ID:
            printf("LD %d\n", val);
            break;
        case NUM:
            printf("LDC %d\n", val);
            break;

		/*-----산술 연산자-----*/
        case ADD: printf("ADD\n"); break;
        case SUB: printf("SUB\n"); break;
        case MUL: printf("MUL\n"); break;
        case DIV: printf("DIV\n"); break;
        case MOD: printf("MOD\n"); break;

		/*-----관계 연산자-----*/
        case GREATER: printf("GT\n"); break;
        case LESS: printf("LT\n"); break;
        case GREATEREQUAL: printf("GE\n"); break;
        case LESSEQUAL: printf("LE\n"); break;
        case EQUAL: printf("EQ\n"); break;
        case NOTEQUAL: printf("NE\n"); break;

		/*-----논리 연산자-----*/
        case AND: printf("AND\n"); break;
        case OR: printf("OR\n"); break;
        case NOT: printf("NOT\n"); break;

		/*-----비트 연산자-----*/
        case BITAND: printf("BAND\n"); break;
        case BITOR: printf("BOR\n"); break;
        case BITXOR: printf("BXOR\n"); break;
        case BITNOT: printf("BNOT\n"); break;
        case RSHIFT: printf("SHR\n"); break;
        case LSHIFT: printf("SHL\n"); break;

		/*-----증감 연산자-----*/
        case INCREASE: printf("INC\n"); break;
        case DECREASE: printf("DEC\n"); break;

		/*-----대입연산자-----*/
        case ASSGN:
            printf("ST\n");
            break;

		/*-----복합 대입연산자-----*/
        case ASSGNADD: printf("ADD\nST\n"); break;
        case ASSGNSUB: printf("SUB\nST\n"); break;
        case ASSGNMUL: printf("MUL\nST\n"); break;
        case ASSGNDIV: printf("DIV\nST\n"); break;
        case ASSGNMOD: printf("MOD\nST\n"); break;

        case IF:
            printf("IF\n");
            break;
        case ELSE:
            printf("ELSE\n");
            break;
        case WHILE:
            printf("WHILE\n");
            break;

        case RETURN:
            printf("RET\n");
            break;
			
        case MAIN:
            printf("MAIN\n");
            break;

        default:
            printf("UNKNOWN TOKEN: %d\n", token);
            break;
    }


/*
int gentemp()
{
char buffer[MAXTSYMLEN];
char tempsym[MAXSYMLEN]="TTCBU";

	tsymbolcnt++;
	if (tsymbolcnt > MAXTSYMBOL) printf("temp symbol overflow\n");
	itoa(tsymbolcnt, buffer, 10);
	strcat(tempsym, buffer);
	return( insertsym(tempsym) ); // Warning: duplicated symbol is not checked for lazy implementation
}
*/
void dwgen()
{
int i;
	fprintf(fp, "HALT\n");
	fprintf(fp, "$ -- END OF EXECUTION CODE AND START OF VAR DEFINITIONS --\n");

// Warning: this code should be different if variable declaration is supported in the language 
	for(i=0; i<maxsym; i++) 
		fprintf(fp, "DW %s\n", symtbl[i]);
	fprintf(fp, "END\n");
}

