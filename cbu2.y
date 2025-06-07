%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4996)

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

// variable declaration

int tsymbolcnt=0;
int errorcnt=0;

int label_count = 0;
int condition_label = 0;

int while_count=0;

int for_count=0;
int for_end_count=0;
int callback_counter=0;

int plusplus=1;
int minusminus=1;

int count=0;


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
Node *MakeCompareNode(int, Node*, Node*);
Node * Makewhiletree( Node *, Node *,Node *);

void	dwgen();
int	gentemp();
void	assgnstmt(int, int);
void	numassgn(int, int);
void	addstmt(int, int, int);
void	substmt(int, int, int);
int		insertsym(char *);
%}


%token	ADD SUB MUL DIV MOD ASSGN ASSGNADD ASSGNSUB ASSGNMUL ASSGNDIV ID NUM STMTEND START END ID2 DECID
%token  EQUAL NOTEQUAL GREATEREQUAL GREATER LESS LESSEQUAL INCREASE DECREASE
%token  LEFTBLOCK RIGHTBLOCK LEFTPAR RIGHTPAR IF ELSE IFELSE CONDELESS CONDGREATER CONDLESSEQUAL CONDGREATEREQUAL CONDEQUAL CONDNOTEQUAL WHILE INITCONDCOMPARE
%token FOR STMTFOR FORCON FORSTART LOOPEND 
%token  FORSENTENCE FOREND
%token ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN ZERO

//DECID : 할당문 선언
//FOREND: for문 종료

%left MUL DIV

%left ADD SUB

%right ASSGN


%%
program : START stmt_list END { if (errorcnt == 0) { codegen($2); dwgen(); } }
    ;

stmt_list : stmt_list stmt { $$ = MakeListTree($1, $2); }
    | stmt { $$ = MakeListTree(NULL, $1); }
    | error STMTEND { errorcnt++; yyerrok; }
    ;


stmt : ID ASSGN expr STMTEND { $1->token = ID2; $$ = MakeOPTree(ASSGN, $1, $3); }
    | compare STMTEND { $$ = $1; }
	| ID ASSGN compare STMTEND{ $1->token = ID2; $$ = MakeOPTree(ASSGN, $1, $3); }
	| while_opration
	| FOR for_condition for_opration	{ $$=MakeOPTree(FOR, $2, $3); }
	| if_sentence
	| assgn_oprator
	| plusplus_minusminus
    ;

assgn_oprator : ID ASSGNADD expr STMTEND {$1->token = DECID; $$=MakeOPTree(ASSGNADD, $1, $3);}
	          | ID ASSGNSUB expr STMTEND {$1->token = DECID; $$=MakeOPTree(ASSGNSUB, $1, $3);}
	          | ID ASSGNMUL expr STMTEND {$1->token = DECID; $$=MakeOPTree(ASSGNMUL, $1, $3);}
	          | ID ASSGNDIV expr STMTEND {$1->token = DECID; $$=MakeOPTree(ASSGNDIV, $1, $3);}
			  ;


plusplus_minusminus :	ID INCREASE STMTEND { $1->token = DECID; $$=MakeOPTree(INCREASE, $1, NULL);}
		            |	ID DECREASE STMTEND { $1->token = DECID; $$=MakeOPTree(DECREASE, $1, NULL);}
			        ;


expr : expr ADD term { $$ = MakeOPTree(ADD, $1, $3); }
     | expr SUB term { $$ = MakeOPTree(SUB, $1, $3); }
     | term
     ;


term : term MUL fact { $$ = MakeOPTree(MUL, $1, $3); }
     | term DIV fact { $$ = MakeOPTree(DIV, $1, $3); }
	 | term MOD fact { $$ = MakeOPTree(MOD, $1, $3); }
     | fact
     ;

fact : ID { /* ID node is created in lex */ }
       | NUM { /* NUM node is created in lex */ }
	   | NUMBER
       ;

NUMBER : decimal_NUMBER TEN decimal_NUMBER { $$ = MakeNode(NUM, $1->tokenval * 10 + $3->tokenval); }
       | decimal_NUMBER TEN { $$ = MakeNode(NUM, $1->tokenval * 10); }
       | decimal_NUMBER { $$ = $1; }
       ;

decimal_NUMBER : ONE { $$ = MakeNode(NUM, 1); }
    | TWO { $$ = MakeNode(NUM, 2); }
    | THREE { $$ = MakeNode(NUM, 3); }
    | FOUR { $$ = MakeNode(NUM, 4); }
    | FIVE { $$ = MakeNode(NUM, 5); }
    | SIX { $$ = MakeNode(NUM, 6); }
    | SEVEN { $$ = MakeNode(NUM, 7); }
    | EIGHT { $$ = MakeNode(NUM, 8); }
    | NINE { $$ = MakeNode(NUM, 9); }
	| ZERO {$$ = MakeNode(NUM, 0);}
    ;

if_sentence : if_stmt
	        | if_stmt else_stmt { $$=MakeOPTree(IFELSE, $1, $2); }
			;

if_stmt :	IF LEFTPAR cond_compare RIGHTPAR LEFTBLOCK stmt_list RIGHTBLOCK { $$=MakeOPTree(IF, $3, $6); }
        ;

else_stmt	:	ELSE LEFTBLOCK stmt_list RIGHTBLOCK { $$=MakeOPTree(ELSE, $3, NULL); }
		    ;


compare : term EQUAL term { $$ = MakeCompareNode(EQUAL, $1, $3); }
        | term NOTEQUAL term { $$ = MakeCompareNode(NOTEQUAL, $1, $3); }
        | term GREATEREQUAL term { $$ = MakeCompareNode(GREATEREQUAL, $1, $3); }
        | term GREATER term { $$ = MakeCompareNode(GREATER, $1, $3); }
        | term LESS term { $$ = MakeCompareNode(LESS, $1, $3); }
        | term LESSEQUAL term { $$ = MakeCompareNode(LESSEQUAL, $1, $3); }
        ;

cond_compare:    expr CONDEQUAL expr { $$=MakeOPTree(CONDEQUAL, $1, $3);}
		    |    expr CONDELESS expr { $$=MakeOPTree(CONDELESS, $1, $3);}
		    |    expr CONDGREATER expr { $$=MakeOPTree(CONDGREATER, $1, $3);}
		    |    expr CONDLESSEQUAL expr { $$=MakeOPTree(CONDLESSEQUAL, $1, $3);}
		    |    expr CONDGREATEREQUAL expr { $$=MakeOPTree(CONDGREATEREQUAL, $1, $3);}
		    |    expr CONDNOTEQUAL expr { $$=MakeOPTree(CONDNOTEQUAL, $1, $3);}
		    ;


while_opration  : WHILE LEFTPAR while_end  { $$=MakeOPTree(WHILE, $2, NULL); }

while_end	: init_cond_compare RIGHTPAR LEFTBLOCK stmt_list RIGHTBLOCK  { $$=MakeListTree($1, $5); }


for_opration	:	stmt RIGHTPAR LEFTBLOCK stmt_list RIGHTBLOCK	{$$=MakeOPTree(STMTFOR, $4, $1);}
	         	;
for_condition		:	LEFTPAR for_begin cond_compare STMTEND	{$$=MakeOPTree(FORCON, $2, $3);}
	    	;
for_begin		:	stmt for_end	{$$=MakeOPTree(FORSTART, $1, $2);}
	    	    ;
for_list 	:	stmt_list loop_end	   {$$=MakeOPTree(FORSENTENCE, $2, $1);}
	    	    ;
for_end		:	{ $$=MakeNode(LOOPEND, LOOPEND); }
loop_end		:	{ $$=MakeNode(FOREND, FOREND); }
            ;

init_cond_compare : cond_compare { $$=MakeOPTree(INITCONDCOMPARE, $1, NULL); }

%%
int main(int argc, char *argv[]) 
{
	printf("#2022041052 Kimgeumyoung compiler project 2024 \n");
	printf("gam language\n");
	
	if (argc == 2)
		yyin = fopen(argv[1], "r");
	else {
		printf("Usage: cbu2 inputfile\noutput file is 'a.asm'\n");
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

Node* MakeCompareNode(int token, Node* arg1, Node* arg2){
    Node* newnode = MakeNode(token, token);
    newnode->son = arg1;
    arg1->brother = arg2;
    return newnode;
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
	Node * brother;
	if (n==NULL) return;
	DFSTree(n->son);
	if (n->token == IF) {
	if(n->brother != NULL) {
	if(n->brother->token == ELSE) {
	fprintf(fp, "GOTO L%d\n", count+1);
			}
		}
	}
	prtcode(n->token, n->tokenval);
	DFSTree(n->brother);
	
}

void prtcode(int token, int val)
{
	switch (token) {
	case ID:
		fprintf(fp,"RVALUE %s\n", symtbl[val]);
		break;
	case ID2:
		fprintf(fp, "LVALUE %s\n", symtbl[val]);
		break;
	case NUM:
		fprintf(fp, "PUSH %d\n", val);
		break;
	case DECID:
		fprintf(fp, "LVALUE %s\n", symtbl[val]); 
		fprintf(fp, "RVALUE %s\n", symtbl[val]);
		break;
	case ADD:
		fprintf(fp, "+\n");
		break;
	case SUB:
		fprintf(fp, "-\n");
		break;
	case MUL:
		fprintf(fp, "*\n");
		break;
	case DIV:
		fprintf(fp, "/\n");
		break;
	case ASSGN:
		fprintf(fp, ":=\n");
		break;
	case ASSGNADD:
		fprintf(fp, "+\n");
		fprintf(fp, ":=\n");
		break;
	case ASSGNSUB:
		fprintf(fp, "-\n");
		fprintf(fp, ":=\n");
		break;
	case ASSGNMUL:
		fprintf(fp, "*\n");
		fprintf(fp, ":=\n");
		break;
	case ASSGNDIV:
		fprintf(fp, "/\n");
		fprintf(fp, ":=\n");
		break;
	case INCREASE:
		fprintf(fp, "PUSH %d\n", plusplus);
		fprintf(fp, "+\n");
		fprintf(fp, ":=\n");
		break;
	case DECREASE:
		fprintf(fp, "PUSH %d\n", minusminus);
		fprintf(fp, "-\n");
		fprintf(fp, ":=\n");
		break;
    case EQUAL:
	    fprintf(fp, "-\n");
        fprintf(fp, "GOFALSE label%d\n", label_count);
        fprintf(fp, "PUSH 0\n"); 
        fprintf(fp, "GOTO label%d\n", label_count + 1);
        fprintf(fp, "LABEL label%d\n", label_count++);
        fprintf(fp, "PUSH 1\n"); 
        fprintf(fp, "LABEL label%d\n", label_count++);
        break;

    case NOTEQUAL:
    	fprintf(fp, "-\n");
        fprintf(fp, "GOFALSE label%d\n", label_count);
        fprintf(fp, "PUSH 1\n"); 
        fprintf(fp, "GOTO label%d\n", label_count + 1);
        fprintf(fp, "LABEL label%d\n", label_count++);
        fprintf(fp, "PUSH 0\n"); 
        fprintf(fp, "LABEL label%d\n", label_count++);
        break;
	
    case GREATER:
        fprintf(fp, "-\n");
        fprintf(fp, "GOPLUS condition%d\n", condition_label);
        fprintf(fp, "PUSH 0\n");
        fprintf(fp, "GOTO end%d\n", label_count);
        fprintf(fp, "LABEL condition%d\n", condition_label++);
        fprintf(fp, "PUSH 1\n");
        fprintf(fp, "LABEL end%d\n", label_count++);
        break;
    case LESS:
        fprintf(fp, "-\n");
        fprintf(fp, "GOMINUS condition%d\n", condition_label);
        fprintf(fp, "PUSH 0\n");
        fprintf(fp, "GOTO end%d\n", label_count);
        fprintf(fp, "LABEL condition%d\n", condition_label++);
        fprintf(fp, "PUSH 1\n");
        fprintf(fp, "LABEL end%d\n", label_count++);
        break;
    case GREATEREQUAL:
        fprintf(fp, "-\n");
        fprintf(fp, "GOPLUS condition%d\n", condition_label);
        fprintf(fp, "PUSH 0\n");
        fprintf(fp, "GOTO end%d\n", label_count);
        fprintf(fp, "LABEL condition%d\n", condition_label++);
        fprintf(fp, "PUSH 1\n");
        fprintf(fp, "LABEL end%d\n", label_count++);
        break;
    case LESSEQUAL:
        fprintf(fp, "-\n");
        fprintf(fp, "GOMINUS condition%d\n", condition_label);
        fprintf(fp, "PUSH 0\n");
        fprintf(fp, "GOTO end%d\n", label_count);
        fprintf(fp, "LABEL condition%d\n", condition_label++);
        fprintf(fp, "PUSH 1\n");
        fprintf(fp, "LABEL end%d\n", label_count++);
        break;
    case IF:
      		fprintf(fp, "LABEL L%d\n", count++);
      		break;
	case IFELSE:
		fprintf(fp, "LABEL L%d\n", count);
		break;
	case CONDEQUAL:
		count++;
      		fprintf(fp, "-\n");
      		fprintf(fp, "GOTRUE L%d\n", count);
      		break;
   	case CONDNOTEQUAL:
		count++;
      		fprintf(fp, "-\n");
      		fprintf(fp, "GOFALSE L%d\n", count);
      		break;
	case CONDELESS:
		count++;
		fprintf(fp, "-\n");
		fprintf(fp, "COPY\n");
		fprintf(fp, "GOPLUS L%d\n", count);
		fprintf(fp, "GOFALSE L%d\n", count);
		break;
	case CONDGREATER:
		count++;
		fprintf(fp, "-\n");
		fprintf(fp, "COPY\n");
		fprintf(fp, "GOMINUS L%d\n", count);
		fprintf(fp, "GOFALSE L%d\n", count);
		break;
	case CONDLESSEQUAL:
		count++;
		fprintf(fp, "-\n");
		fprintf(fp, "GOPLUS L%d\n", count);
		break;
	case CONDGREATEREQUAL:
		count++;
		fprintf(fp, "-\n");
		fprintf(fp, "GOMINUS L%d\n", count);
		break;

	case WHILE:
		fprintf(fp, "GOTO loop%d\n", while_count-1);
		fprintf(fp, "LABEL label%d\n", label_count++);
		break;

	case FOR:
		fprintf(fp, "GOTO FLOOP%d\n", for_count);   
		fprintf(fp, "LABEL L%d\n", count);
		count--;
		break;
	case LOOPEND:
		fprintf(fp, "LABEL FLOOP%d\n", ++for_count);  
		break;
	case FOREND:
		fprintf(fp, "GOTO FLABEL%d\n", ++for_end_count);    
		fprintf(fp, "LABEL CALLBACK%d\n", callback_counter);
		callback_counter--;
		break;
	case STMTLIST:
	default:
		break;
	};
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

