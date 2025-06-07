%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define DEBUG	1

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
extern YYSTYPE yylval;

int tsymbolcnt=0;
int errorcnt=0;

extern FILE *yyin; FILE *fp;

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
int yylex(void);
void yyerror(const char *s);
%}
/*--- 우선순위 ---*/


%right ASSGN
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
%token	INT FLOAT DOUBLE CHAR 
%token  IF ELSE WHILE
%token  GREATER GREATEREQUAL LESS LESSEQUAL EQUAL NOTEQUAL AND OR
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
		| dcl
		;

stmt_list: 		stmt_list stmt 		{$$=MakeListTree($1, $2);}
		| stmt			{$$=MakeListTree(NULL, $1);}
		| error STMTEND		{ errorcnt++; yyerrok;}
		;

stmt: 		expr_stmt
		|	if_stmt
		|	while_stmt
		|	'{' stmt_list '}'
		;
		
expr_stmt:
	  expr STMTEND        { $$ = $1; }
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

		|  ID ASSGN expr       { $1->token = ID2; $$=MakeOPTree(ASSGN, $1, $3); }
		|	term
		;

/*-- term 확인 완료 --*/
term:		ID		{ $$ = $1; }
		|	NUM		{ $$ = $1;}
		;

dcl: ID STMTEND { insertsym(symtbl[$2->tokenval]); }
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

void yyerror(const char *s)
{
    printf("syntax error: %s\n", s);
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

int labelCount = 0;
int newLabel() {
    return labelCount++;
}


void DFSTree(Node *n)
{
    if (n == NULL) return;

    switch (n->token) {
        case IF: {
            int labelElse = newLabel();
            int labelEnd = newLabel();

            Node *cond = n->son; //조건
            Node *thenStmt = cond->brother; //결과
            Node *elseStmt = thenStmt ? thenStmt->brother : NULL; //else 있으면 

            DFSTree(cond); // 조건 평가
            fprintf(fp, "GOFALSE L%d\n", labelElse); //false이면 false로 점프

            DFSTree(thenStmt); // then block
            fprintf(fp, "GOTO L%d\n", labelEnd); //true면 할꺼 하고 end로 점프

            fprintf(fp, "LABEL L%d\n", labelElse); 
            if (elseStmt) DFSTree(elseStmt); // else 점프 도착지

            fprintf(fp, "LABEL L%d\n", labelEnd);
            break;
        }

        case WHILE: {
            int labelStart = newLabel();
            int labelEnd = newLabel();

            Node *cond = n->son; //조건
            Node *body = cond->brother; //

            fprintf(fp, "LABEL L%d\n", labelStart); // 루프 도착지
            DFSTree(cond);  // 조건 평가
            fprintf(fp, "GOFALSE L%d\n", labelEnd); //조건 문 점프

            DFSTree(body);  // 본문
            fprintf(fp, "GOTO L%d\n", labelStart); //루프 점프

            fprintf(fp, "LABEL L%d\n", labelEnd); // 조건문 도착지
            break;
        }

        case ASSGN: {
              
            Node *left = n->son; //ID
            Node *right = left->brother; //value

            DFSTree(right);                // 오른쪽 값을 먼저 평가
            fprintf(fp, "LVALUE %d\n", left->tokenval); // 변수 주소 지정
            fprintf(fp, ":=\n");           // 저장
            break;
        }

        case RETURN: {
            Node *retval = n->son;
            DFSTree(retval);
            fprintf(fp, "RET\n");
            break;
        }

        case STMTLIST: {
            Node *stmt = n->son;
            while (stmt) {
                DFSTree(stmt);
                stmt = stmt->brother;
            }
            break;
        }

        default:
            DFSTree(n->son);
            prtcode(n->token, n->tokenval);
            DFSTree(n->brother);
            break;
    }
}

void prtcode(int token, int val)
{
    switch (token)
    {
        case ID:
            fprintf(fp, "RVALUE %d\n", val);
            break;

        case ID2:
            fprintf(fp, "LVALUE %d\n", val);
            break;			
			
        case NUM:
            fprintf(fp, "LDC %d\n", val);
            break;

        case ADD: fprintf(fp, "ADD\n"); break;
        case SUB: fprintf(fp, "SUB\n"); break;
        case MUL: fprintf(fp, "MUL\n"); break;
        case DIV: fprintf(fp, "DIV\n"); break;
        case MOD: fprintf(fp, "MOD\n"); break;

        case GREATER: fprintf(fp, "GT\n"); break;
        case LESS: fprintf(fp, "LT\n"); break;
        case GREATEREQUAL: fprintf(fp, "GE\n"); break;
        case LESSEQUAL: fprintf(fp, "LE\n"); break;
        case EQUAL: fprintf(fp, "EQ\n"); break;
        case NOTEQUAL: fprintf(fp, "NE\n"); break;

        case AND: fprintf(fp, "AND\n"); break;
        case OR: fprintf(fp, "OR\n"); break;
        case NOT: fprintf(fp, "NOT\n"); break;

        case BITAND: fprintf(fp, "BAND\n"); break;
        case BITOR: fprintf(fp, "BOR\n"); break;
        case BITXOR: fprintf(fp, "BXOR\n"); break;
        case BITNOT: fprintf(fp, "BNOT\n"); break;
        case RSHIFT: fprintf(fp, "SHR\n"); break;
        case LSHIFT: fprintf(fp, "SHL\n"); break;

        case INCREASE: fprintf(fp, "INC\n"); break;
        case DECREASE: fprintf(fp, "DEC\n"); break;

        default:
            fprintf(fp, "; Wrong Token %d\n", token);
            break;
    }
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

