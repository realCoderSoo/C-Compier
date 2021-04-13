/*	File name: parser.h
	Compiler: MS Visual Studio 2019
	Author: Soojin Han, 040698591
	Course: CST 8152 – Compilers, Lab Section: 012
	Assignment: 3
	Date: August 7, 2020
	Professor: Paulo Sousa
	Purpose: To understand the structure of buffer system and command line arguments to send output with buffer operational mode.
	Function list: malar_next_token(), parser(), match(),syn_eh(), syn_printe(), gen_incode(), program(), input_statement(), 
	opt_statements(), statement(), statements(), variable_list(), output_list(), output_statement(), variable_identifier(),
	multi_variable_list(), iteration_statement(), conditional_expression(), selection_statement(), primary_arithmetic_expression(),
	primary_s_relational_expression(), multi_primary_s_relational_expression(), primary_a_relational_expression(),
	multi_primary_a_relational_expression(), relational_expression(), multi_logical_and_expression(), logical_and_expression()
	multi_logical_or_expression(), logical_or_expression(), pre_condition(), assignment_statement(), assignment_expression(),
	string_expression(), arithmetic_expression(), primary_string_expression(), multi_string_expression(),
	unary_arithmetic_expression(), additive_arithmetic_expression(), multiplicative_arithmetic_expression(), multi_statement(),
	multi_additive_arithmetic_expression(), multi_multiplicative_arithmetic_expression() */

#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#define ELSE 0		/*else kw code 0*/
#define FALSE 1		/*false kw code 1*/
#define IF 2		/*if kw code 2*/
#define PLATYPUS 3	/*platypus kw code 3*/
#define READ 4		/*read kw code 4*/
#define REPEAT 5	/*repeat kw code 5*/
#define THEN 6		/*then kw code 6*/
#define TRUE 7		/*true kw code 7*/
#define WHILE 8		/*while kw code 8*/
#define WRITE 9		/*write kw code 9*/
#define NO_ATTR -1	/*no_attribute code -1*/

static Token lookahead;				/*to get a token status*/
extern Token malar_next_token(void);/*get next token*/
extern int line;					/* current line */
extern Buffer* str_LTBL;			/* buffer token pointer*/
extern char* kw_table[];			/* keyword table */
int synerrno;						/* error count */

/*parser grammar methods*/
void parser(void);
void match(int, int);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program(void);
void input_statement(void);
void opt_statements(void);
void statement(void);
void statements(void);
void variable_list(void);
void output_list(void);
void output_statement(void);
void variable_identifier(void);
void multi_variable_list(void);
void iteration_statement(void);
void conditional_expression(void);
void selection_statement(void);
void primary_arithmetic_expression(void);
void primary_s_relational_expression(void);
void multi_primary_s_relational_expression(void);
void primary_a_relational_expression(void);
void multi_primary_a_relational_expression(void);
void relational_expression(void);
void multi_logical_and_expression(void);
void logical_and_expression(void);
void multi_logical_or_expression(void);
void logical_or_expression(void);
void pre_condition(void);
void assignment_statement(void);
void assignment_expression(void);
void string_expression(void);
void arithmetic_expression(void);
void primary_string_expression(void);
void multi_string_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void multiplicative_arithmetic_expression(void);
void multi_statement(void);
void multi_additive_arithmetic_expression(void);
void multi_multiplicative_arithmetic_expression(void);

#endif