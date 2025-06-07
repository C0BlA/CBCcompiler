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

void dwgen();

int newLabel();
int insertsym(char *);
int yylex(void);
void yyerror(const char *s);
%}

/* 우선순위 및 연산자 */
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

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%token MAIN ASSGN ID NUM STMTEND START END ID2 
%token ADD SUB MUL DIV MOD
%token INT FLOAT DOUBLE CHAR 
%token IF ELSE WHILE
%token GREATER GREATEREQUAL LESS LESSEQUAL EQUAL NOTEQUAL AND OR
%token RETURN 
%token INCREASE DECREASE
%token BITAND BITOR BITXOR BITNOT
%token RSHIFT LSHIFT

%%

program:
	functions {
	if (errorcnt == 0) {
		printf("✅ codegen 진입\n");
		codegen($1);
		dwgen();
	} else {
		printf("❌ errorcnt = %d, 코드 생성 안함\n", errorcnt);
	}
	}
;

functions:
	functions function
	| function
	;

function:
	fun_header fun_body { $$ = $2; }
	;

fun_header:
	MAIN '(' ')'
	;

fun_body:
	'{' dcl_list stmt_list '}' { $$ = $3; }
	;

dcl_list:
	dcl_list dcl
	| dcl
	;

dcl:
	ID STMTEND { insertsym(symtbl[$1->tokenval]); $$ = NULL; }

stmt_list:
	stmt_list stmt { $$ = MakeListTree($1, $2); }
	| stmt { $$ = MakeListTree(NULL, $1); }
	| error STMTEND { errorcnt++; yyerrok; }
	;

stmt:
	expr_stmt
	| if_stmt
	| while_stmt
	| '{' stmt_list '}'
	;

expr_stmt:
	expr STMTEND { $$ = $1; }
	;

expr
	: '(' expr ')' { $$ = $2; }
	| expr ADD term { $$ = MakeOPTree(ADD, $1, $3); }
	| expr SUB term { $$ = MakeOPTree(SUB, $1, $3); }
	| term
	;

term
	: term MUL factor { $$ = MakeOPTree(MUL, $1, $3); }
	| term DIV factor { $$ = MakeOPTree(DIV, $1, $3); }
	| term MOD factor { $$ = MakeOPTree(MOD, $1, $3); }
	| factor
	;

factor
	: BITNOT factor { $$ = MakeOPTree(BITNOT, $2, NULL); }
	| NOT factor { $$ = MakeOPTree(NOT, $2, NULL); }
	| INCREASE factor { $$ = MakeOPTree(INCREASE, $2, NULL); }
	| DECREASE factor { $$ = MakeOPTree(DECREASE, $2, NULL); }
	| primary
	;

primary
	: ID ASSGN expr { $1->token = ID2; $$ = MakeOPTree(ASSGN, $1, $3); }
	| ID { $$ = $1; }
	| NUM { $$ = $1; }
	;
	
if_stmt:
	IF '(' expr ')' stmt %prec LOWER_THAN_ELSE { $$ = MakeOPTree(IF, $3, $5); }
	| IF '(' expr ')' stmt ELSE stmt { $$ = MakeOPTree(ELSE, MakeOPTree(IF, $3, $5), $7); }
	;

while_stmt:
	WHILE '(' expr ')' stmt { $$ = MakeOPTree(WHILE, $3, $5); }
	;

%%

int main(int argc, char *argv[]) 
{
	printf("\nsample CBU C compiler v3.0\n");
	printf("(C) Copyright by Jae Sung Lee (jasonlee@cbnu.ac.kr), 2025.\n");

	if (argc == 2)
		yyin = fopen(argv[1], "r");
	else {
		printf("Usage: cbc3 inputfile\noutput file is 'a.asm'\n");
		return 0;
	}

	fp = fopen("a.asm", "w");
	yyparse();
	fclose(yyin);
	fclose(fp);

	if (errorcnt == 0)
		printf("Successfully compiled. Assembly code is in 'a.asm'.\n");
}

void yyerror(const char *s)
{
	printf("❌ syntax error at line %d: %s\n", lineno, s);
	errorcnt++;
}

Node *MakeOPTree(int op, Node *operand1, Node *operand2)
{
	Node *newnode = (Node *)malloc(sizeof(Node));
	newnode->token = op;
	newnode->tokenval = op;
	newnode->son = operand1;
	newnode->brother = NULL;
	if (operand1) operand1->brother = operand2;
	return newnode;
}

Node *MakeNode(int token, int operand)
{
	Node *newnode = (Node *)malloc(sizeof(Node));
	newnode->token = token;
	newnode->tokenval = operand;
	newnode->son = newnode->brother = NULL;
	return newnode;
}

Node *MakeListTree(Node *operand1, Node *operand2)
{
	if (operand1 == NULL) {
		Node *newnode = (Node *)malloc(sizeof(Node));
		newnode->token = newnode->tokenval = STMTLIST;
		newnode->son = operand2;
		newnode->brother = NULL;
		return newnode;
	} else {
		Node *node = operand1->son;
		while (node->brother != NULL) node = node->brother;
		node->brother = operand2;
		return operand1;
	}
}

int labelCount = 0;
int newLabel() { return labelCount++; }

void codegen(Node *root) { DFSTree(root); }

void DFSTree(Node *n)
{
	if (n == NULL) return;

	switch (n->token) {
		case IF: {
			int labelElse = newLabel();
			int labelEnd = newLabel();
			Node *cond = n->son;
			Node *thenStmt = cond->brother;
			Node *elseStmt = thenStmt ? thenStmt->brother : NULL;
			DFSTree(cond);
			fprintf(fp, "GOFALSE L%d\n", labelElse);
			DFSTree(thenStmt);
			fprintf(fp, "GOTO L%d\n", labelEnd);
			fprintf(fp, "LABEL L%d\n", labelElse);
			if (elseStmt) DFSTree(elseStmt);
			fprintf(fp, "LABEL L%d\n", labelEnd);
			break;
		}
		case WHILE: {
			int labelStart = newLabel();
			int labelEnd = newLabel();
			Node *cond = n->son;
			Node *body = cond->brother;
			fprintf(fp, "LABEL L%d\n", labelStart);
			DFSTree(cond);
			fprintf(fp, "GOFALSE L%d\n", labelEnd);
			DFSTree(body);
			fprintf(fp, "GOTO L%d\n", labelStart);
			fprintf(fp, "LABEL L%d\n", labelEnd);
			break;
		}
		case ASSGN: {
			Node *left = n->son;
			Node *right = left->brother;
			DFSTree(right);
			fprintf(fp, "LVALUE %d\n", left->tokenval);
			fprintf(fp, ":=\n");
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
		default: {
			Node *left = n->son;
			Node *right = left ? left->brother : NULL;

			DFSTree(left);
			if (right) DFSTree(right);

			prtcode(n->token, n->tokenval);
			DFSTree(n->brother);
			break;
		}
	}
}

void prtcode(int token, int val)
{
	switch (token) {
		case ID: fprintf(fp, "RVALUE %d\n", val); break;
		case ID2: fprintf(fp, "LVALUE %d\n", val); break;
		case NUM: fprintf(fp, "LDC %d\n", val); break;
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
		default: fprintf(fp, "; Wrong Token %d\n", token); break;
	}
}

void dwgen()
{
	fprintf(fp, "HALT\n");
	fprintf(fp, "$ -- END OF EXECUTION CODE AND START OF VAR DEFINITIONS --\n");
	for (int i = 0; i < maxsym; i++)
		fprintf(fp, "DW %s\n", symtbl[i]);
	fprintf(fp, "END\n");
}
