/*	File name: parser.c
	Compiler: MS Visual Studio 2019
	Author: Soojin Han, 040698591
	Course: CST 8152 – Compilers, Lab Section: 012
	Assignment: 3
	Date: August 7, 2020
	Professor: Paulo Sousa
	Purpose: To understand the structure of buffer system and command line arguments to send output with buffer operational mode.
	Function list: parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(), input_statement(), output_list(),
	opt_variable_list(), output_statement(), variable_identifier(), multi_variable_list(), variable_list(), opt_statements(),
	iteration_statement(), statement(), multi_statement(), statements(), string_expression(), multi_string_expression(),
	primary_string_expression(), primary_arithmetic_expression(),unary_arithmetic_expression(),
	multi_additive_arithmetic_expression(),additive_arithmetic_expression(),multi_multiplicative_arithmetic_expression(),
	multiplicative_arithmetic_expression(),arithmetic_expression(),assignment_expression(), assignment_statement(),
	primary_s_relational_expression(),multi_primary_s_relational_expression(),primary_a_relational_expression(),
	multi_primary_a_relational_expression(),relational_expression(),multi_logical_and_expression(),logical_and_expression(),
	multi_logical_or_expression(),logical_or_expression(),conditional_expression(),pre_condition(),selection_statement()*/

#include "parser.h"
#include <stdlib.h>
#include <string.h>

/*
* Purpose: This function implements <input statement> -> READ (<variable list>);
* FIRST(<input statement>) = { KW_T(READ) } */
void input_statement(void)
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();/*<variable list> -> <variable identifier> <variable list’>*/
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*
* Purpose: This function implements <output_list> -> <opt_variable list> | STR_T;
* FIRST (<output_list>) = { FIRST (<opt_variable list >) , STR_T} = {AVID_T, SVID_T, STR_T, ϵ } */
void output_list(void)
{
	/*it checks if token code is AVID_T or SVID_T and calls variable_list()*/
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		variable_list();
	}
	else if (lookahead.code == STR_T)
	{
		/*string literal token check*/
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
	}
	else
	{
		/*empty output list*/
		gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*
 * Purpose: This function implements <output statement> -> WRITE (<output list>);
* FIRST(<output statement>) = { KW_T(WRITE) } */
void output_statement(void)
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();/*<output_list> -> <opt_variable list> | STR_T;*/
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*
* Purpose: This function implements <variable identifier> -> AVID_T | SVID_T
* FIRST(<variable identifier>) = { AVID_T, SVID_T } */
void variable_identifier(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		/*checks if token code is either AVID_T or SVID_T*/
		match(lookahead.code, NO_ATTR);
	}
	else
	{
		/*error statements*/
		syn_printe();
	}
}

/*
* Purpose: This function implements , <variable identifier><variable list’> | ϵ
* FIRST(<variable list’>) = { COM_T, ϵ } */
void multi_variable_list(void)
{
	if (lookahead.code == COM_T) 
	{
		/*token code starts with a comma*/
		match(COM_T, NO_ATTR);
		variable_identifier();
		multi_variable_list();
	}
}

/*
* Purpose: This function implements <variable list> -> <variable identifier> <variable list’>
* FIRST(<variable list>) = { FIRST(<variable identifier>) } = { AVID_T, SVID_T } */
void variable_list(void)
{
	variable_identifier();
	multi_variable_list();
	gen_incode("PLATY: Variable list parsed");
}

/*
* Purpose: This function implements <opt_statements> -> <statements> | ϵ
* FIRST(<opt_statements>) = { FIRST(<statements>) } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE),
	KW_T(READ), KW_T(WRITE), ϵ}
*/
void opt_statements(void)
{
	/*checks token code*/
	if (lookahead.code == KW_T)
	{
		if (lookahead.attribute.get_int == IF || lookahead.attribute.get_int == WHILE ||
			lookahead.attribute.get_int == READ || lookahead.attribute.get_int == WRITE)
		{
			statements();/*<statements> -> <statement><statements'>*/
		}
		else
		{
			/*empty string statement*/
			gen_incode("PLATY: Opt_statements parsed");
		}
	}
	else if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		statements();/*<statements> -> <statement><statements'>*/
	}
	else
	{
		/*empty string statement*/
		gen_incode("PLATY: Opt_statements parsed");
	}

}

/*
* Purpose: This function implements <iteration statement> -> WHILE <pre-condition> (<conditional expression>)
															 REPEAT { <statements>};
* FIRST(<iteration statement>) = { KW_T(WHILE) }*/
void iteration_statement(void)
{
	match(KW_T, WHILE);
	pre_condition();/*<pre-condition> -> TRUE | FALSE*/
	match(LPR_T, NO_ATTR);
	conditional_expression();/*<conditional expression> -> <logical OR  expression>*/
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();/*<statements> -> <statement><statements'>*/
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/*
* Purpose: This function implements <statement> -><assignment statement>
										| <selection statement>
										| <iteration statement>
										| <input statement>
										| <output statement>
* FIRST(<statement>) = { FIRST(<assignment statement>), FIRST(<selection statements>), FIRST(<iteration statement>),
	FIRST(<input statement>), FIRST(<output statement>) } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
	KW_T(WRITE) }
 */
void statement(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		/*<assignment statement> -> <assignment expression> ;*/
		assignment_statement();
	}
	else if (lookahead.code == KW_T)
	{
		/*token code is KW_T so it checks for its attribute code*/
		switch (lookahead.attribute.get_int)
		{
		case IF:
			selection_statement();
			break;
		case WHILE:
			iteration_statement();
			break;
		case READ:
			input_statement();
			break;
		case WRITE:
			output_statement();
			break;
		default:
			break;
		}
	}
	else
	{
		/*error statements*/
		syn_printe();
	}
}

/*
* Purpose: This function implements <statements'> -> <statement><statements'> | ϵ
* FIRST(<statements’>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) , ϵ } */
void multi_statement(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		/*token code AVID_T or SVID_T*/
		statement();
		multi_statement();
	}
	else if (lookahead.code == KW_T)
	{
		/*token code is KW_T*/
		if (lookahead.attribute.get_int == IF || lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ || lookahead.attribute.get_int == WRITE)
		{
			statements();
		}
	}
}

/*
* Purpose: This function implements <statements> -> <statement><statements'>
* FIRST(<statements>) = { FIRST(<statement>) } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
	KW_T(WRITE) }  */
void statements(void)
{
	statement();
	multi_statement();
}

/*
* Purpose: This function implements <string expression> -> <primary string expression> | <string expression’>
* FIRST(<string expression>) = {FIRST(<primary string expression >)}  = {SVID_T,STR_T}*/
void string_expression(void)
{
	primary_string_expression();
	multi_string_expression();
	gen_incode("PLATY: String expression parsed");
}

/*
* Purpose: This function implements <string expression’> -> ##  <primary string expression>
															<string expression’> | ϵ
* FIRST(<string expression’>) = { ##, ϵ } */
void multi_string_expression(void)
{
	if (lookahead.code == SCC_OP_T)
	{
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		multi_string_expression();
	}
}

/*
* Purpose: This function implements <primary string expression> -> SVID_T | STR_T
* FIRST(<primary string expression>) = { SVID_T, STR_T }*/
void primary_string_expression(void)
{
	if (lookahead.code == SVID_T)
	{
		match(SVID_T, NO_ATTR);/*checking for SVID_T*/
	}

	if (lookahead.code == STR_T)
	{
		match(STR_T, NO_ATTR);/*checking for STR_T*/
	}

	gen_incode("PLATY: Primary string expression parsed");
}

/*
* Purpose: This function implements <primary arithmetic expression> -> AVID_T | FPL_T | INL_T
																	 | (<arithmetic expression>)
* FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( } */
void primary_arithmetic_expression(void)
{
	if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T)
	{
		match(lookahead.code, NO_ATTR);
	}
	else if (lookahead.code == LPR_T)
	{
		match(LPR_T, NO_ATTR);
		/*<arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>*/
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
	}

	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
* Purpose: This function implements <unary arithmetic expression> -> - <primary arithmetic expression>
																	| + <primary arithmetic expression>
* FIRST(<unary arithmetic expression>) = { -, + }*/
void unary_arithmetic_expression(void)
{
	if (lookahead.code == ART_OP_T)
	{
		/*token code is ART_OP_T*/
		if (lookahead.attribute.arr_op == MINUS)
		{
			/*token attribute arithmetic operator is MINUS*/
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
		}
		else if (lookahead.attribute.arr_op == PLUS)
		{
			/*token attribute arithmetic operator is PLUS*/
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
		}
	}

	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*
* Purpose: This function implements <additive arithmetic expression’> -> + <multiplicative arithmetic expression>
																		<additive arithmetic expression’>
																	   | - <multiplicative arithmetic expression>
																	   <additive arithmetic expression’> | ϵ
* FIRST(<additive arithmetic expression’>) = { +, -, ϵ } */
void multi_additive_arithmetic_expression(void)
{
	if (lookahead.code == ART_OP_T)
	{
		/*token code is arithmetic operator token*/
		if (lookahead.attribute.arr_op == MINUS)
		{
			/*arithmetic operator is MINUS*/
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			multi_additive_arithmetic_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		else if (lookahead.attribute.arr_op == PLUS)
		{
			/*arithmetic operator is PLUS*/
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			multi_additive_arithmetic_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		else
		{
			/*empty string*/
			syn_printe();
		}

	}
}

/*
* Purpose: This function implements <additive arithmetic expression> -> <multiplicative arithmetic expression>
																		<additive arithmetic expression’>
* FIRST(<additive arithmetic expression>) = {FIRST(<multiplicative arithmetic expression>)} =
	{ AVID_T, FPL_T, INL_T, ( } */
void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression();
	multi_additive_arithmetic_expression();
}

/*
* Purpose: This function implements <multiplicative arithmetic expression’> -> * < primary arithmetic expression>
																			  <multiplicative arithmetic expression’>
																			  | / < primary arithmetic expression>
																			  < multiplicative arithmetic expression’>
* FIRST(<multiplicative arithmetic expression’>) = { *, /, ϵ } */
void multi_multiplicative_arithmetic_expression(void)
{
	if (lookahead.code == ART_OP_T)
	{
		/*token code is ART_OP_T*/
		if (lookahead.attribute.arr_op == DIV)
		{
			/*arithmetic operator is division*/
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multi_multiplicative_arithmetic_expression();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
		else if (lookahead.attribute.arr_op == MULT)
		{
			/*arithmetic operator is multiplication*/
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multi_multiplicative_arithmetic_expression();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
	}
}

/*
* Purpose: This function implements <multiplicative arithmetic expression> -> <primary arithmetic expression>
																			  <multiplicative arithmetic expression’>
* FIRST(<multiplicative arithmetic expression >) = { FIRST(<primary arithmetic expression>)}  =
	{ AVID_T, FPL_T, INL_T, ( } */
void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression();
	multi_multiplicative_arithmetic_expression();
}

/*
* Purpose: This function implements <arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>
* FIRST(<arithmetic expression>) = { FIRST(<unary arithmetic expression>,  */
void arithmetic_expression()
{
	if (lookahead.code == ART_OP_T)
	{
		/*token code is ART_OP_T*/
		if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS)
		{
			unary_arithmetic_expression();
		}
		else
		{
			syn_printe();/*error statements*/
		}

		gen_incode("PLATY: Arithmetic expression parsed");
	}
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T || lookahead.code == LPR_T)
	{
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
	}
	else
	{
		syn_printe();
	}

}

/*
* Purpose: This function implements < assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
* FIRST(<assignment expression>) = { AVID_T, SVID_T } */
void assignment_expression(void)
{
	if (lookahead.code == SVID_T)
	{
		/*token code is SVID_T*/
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
	}

	if (lookahead.code == AVID_T)
	{
		/*token code is AVID_T*/
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
	}
}

/*
* Purpose: This function implements <assignment statement> -> <assignment expression> ;
* FIRST(<assignment statement>) = { FIRST(<assignment expression>) } = { AVID_T, SVID_T } */
void assignment_statement(void)
{
	assignment_expression();/*< assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>*/
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
* Purpose: This function implements <primary s_relational expression> -> <primary string expression>
* FIRST(<primary s_relational_expression>) = { FIRST(<primary string expression> } = { SVID_T, STR_T } */
void primary_s_relational_expression(void)
{
	if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T)
	{
		/*token code is  AVID_T,FPL_T, or INL_T*/
		syn_printe();
		gen_incode("PLATY: Primary s_relational expression parsed");
	}
	else if (lookahead.code == SVID_T || lookahead.code == STR_T)
	{
		/*token code is SVID_T or STR_T*/
		primary_string_expression();
		gen_incode("PLATY: Primary s_relational expression parsed");
	}
	else
	{
		syn_printe();
	}
}

/*
* Purpose: This function implements <primary s_relational expression’> -> == <primary s_relational expression>
																		| <> <primary s_relational expression>
																		| > <primary s_relational expression>
																		| < <primary s_relational expression>
* FIRST(<primary s_relational expression’>) = { ==, <>, >, < } */
void multi_primary_s_relational_expression(void)
{
	if (lookahead.code == REL_OP_T)
	{
		/*token code is REL_OP_T*/
		if (lookahead.attribute.rel_op == EQ || lookahead.attribute.rel_op == NE || lookahead.attribute.rel_op == GT 
			|| lookahead.attribute.rel_op == LT)
		{
			/*relational operator attribute code is EQ, NE, GT,or LT*/
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_s_relational_expression();
		}
		else
		{
			syn_printe();
		}
	}
}

/*
* Purpose: This function implements <primary s_relational expression’> -> == <primary s_relational expression>
																		| <> <primary s_relational expression>
																		| > <primary s_relational expression>
																		| < <primary s_relational expression>
* FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T } */
void primary_a_relational_expression(void)
{
	if (lookahead.code == SVID_T || lookahead.code == STR_T)
	{
		syn_printe();
		gen_incode("PLATY: Primary a_relational expression parsed");
	}
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T)
	{
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
	}
	else
	{
		syn_printe();
	}
}

/*
* Purpose: This function implements <primary a_relational expression’> -> == <primary a_relational expression>
																		| <> <primary a_relational expression>
																		| > <primary a_relational expression>
																		| < <primary a_relational expression>
* FIRST(<primary a_relational expression’>) = { ==, <>, >, < } */
void multi_primary_a_relational_expression(void)
{
	if (lookahead.code == REL_OP_T)
	{
		if (lookahead.attribute.rel_op == EQ || lookahead.attribute.rel_op == NE || lookahead.attribute.rel_op == GT 
			|| lookahead.attribute.rel_op == LT)
		{
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_a_relational_expression();
		}
		else
		{
			syn_printe();
		}
	}
}

/*
* Purpose: This function implements <relational expression> -> <primary a_relational expression> <primary a_relational  expression’>
											| <primary s_relational expression> <primary s_relational  expression’>
* FIRST(<relational expression>) = { FIRST(<primary a_relational expression>),  */
void relational_expression(void)
{
	if (lookahead.code == SVID_T || lookahead.code == STR_T)
	{
		primary_s_relational_expression();
		multi_primary_s_relational_expression();
	}
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T)
	{
		primary_a_relational_expression();
		multi_primary_a_relational_expression();
	}
	else
	{
		syn_printe();
	}

	gen_incode("PLATY: Relational expression parsed");
}

/*
* Purpose: This function implements <logical AND expression’> -> .AND.<relational expression><logical AND expression’>
* FIRST(<logical AND expression’>) = { .AND. , ϵ } */
void multi_logical_and_expression(void)
{
	if (lookahead.code == LOG_OP_T)
	{
		if (lookahead.attribute.log_op == AND)
		{
			match(LOG_OP_T, AND);
			relational_expression();
			multi_logical_and_expression();
			gen_incode("PLATY: Logical AND expression parsed");
		}
	}
}

/*
* Purpose: This function implements <logical AND expression> -> <relational expression><logical AND expression’>
* FIRST(<logical AND expression>) = { FIRST(<relational expression>) } = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 */
void logical_and_expression(void)
{
	relational_expression();
	multi_logical_and_expression();
}

/*
* Purpose: This function implements <logical OR expression’> -> .OR. <logical AND expression> <logical OR expression’>
																| ϵ
* FIRST(<logical OR expression’>) = { .OR. , ϵ } */
void multi_logical_or_expression(void)
{
	if (lookahead.code == LOG_OP_T)
	{
		if (lookahead.attribute.log_op == OR)
		{
			match(LOG_OP_T, OR);
			logical_and_expression();
			multi_logical_or_expression();
			gen_incode("PLATY: Logical OR expression parsed");
		}
	}
}

/*
* Purpose: This function implements <logical OR expression> -> <logical AND expression> <logical OR expression’>
* FIRST(<logical OR expression>) = { FIRST(<logical AND expression>) } = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void logical_or_expression(void)
{
	logical_and_expression();
	multi_logical_or_expression();
}

/*
* Purpose: This function implements <conditional expression> -> <logical OR  expression>
* FIRST(<conditional expression>) = { FIRST(<logical OR expression>) } = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void conditional_expression(void)
{
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
* Purpose: This function implements <pre-condition> -> TRUE | FALSE
* FIRST(<pre-condition>) = { TRUE, FALSE } */
void pre_condition(void)
{
	if (lookahead.code == KW_T)
	{
		if (lookahead.attribute.kwt_idx == TRUE)
		{
			/*keyword index is true*/
			match(KW_T, TRUE);
		}
		else
		{
			/*keyword index is false*/
			match(KW_T, FALSE);
		}
	}
}

/*
* Purpose: This function implements <selection statement> -> IF <pre-condition>  (<conditional expression>) THEN
															{ <opt_statements> } ELSE { <opt_statements> } ;
* FIRST(<selection statement>) = { KW_T(IF) } */
void selection_statement(void)
{
	match(KW_T, IF);
	pre_condition();/*<pre-condition> -> TRUE | FALSE*/
	match(LPR_T, NO_ATTR);
	conditional_expression();/*<conditional expression> -> <logical OR  expression>*/
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();/*<opt_statements> -> <statements> | ϵ*/
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*
* Purpose: This function determines the next state of the token, runs program function, and matches end of file
* Author: Soojin Han
* Versions: 1.0 2020-08-07
* Called Functions: malar_next_token(),program(),match(),gen_incode()
* Parameters: void
* Return value: void
* Algorithm: 1. it gets next state token
			 2. it goes into program function
			 3. it matches end of file code
			 4. it prints necessary statement */
void parser(void)
{
	/*gets next token*/
	lookahead = malar_next_token();
	/*calls program function to match program grammar*/
	program();
	/*matching end of file*/
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*
* Purpose: This function determines if token code matches the parameter token code
* Author: Soojin Han
* Versions: 1.0 2020-08-07
* Called Functions: syn_eh(),malar_next_token(),syn_printe()
* Parameters: int pr_token_code, int pr_token_attribute
* Return value: void
* Algorithm: 1. it checks if current token matches the parameter token code, and calls syn_eh() function
				if it doesn't match.
			 2. if they match, it checks for SEOF_T, and continues if it's not SEOF_T
			 3. if code keyword token, it checks if it's in the keyword table(error if not)
			 4. if code is LOG_OP_T,ART_OP_T, or REL_OP_T, it checks if token attributes matches each other
			 5. gets a next token state, and if token code is ERR_T, it calls syn_printe(), gets a next token,
				and increments runtime error count */
void match(int pr_token_code, int pr_token_attribute)
{
	/*checks fot token code match*/
	if (lookahead.code == pr_token_code)
	{
		/*checks for source end of file*/
		if (lookahead.code == SEOF_T)
		{
			return;
		}

		/*checks if token code is KW_T*/
		if (pr_token_code == KW_T)
		{
			/*if token attribute is not the same as keyword index, it calls syn_eh()*/
			if (pr_token_attribute != lookahead.attribute.kwt_idx)
			{
				syn_eh(pr_token_code);
			}

		}
		else if (pr_token_code == LOG_OP_T || pr_token_code == ART_OP_T || pr_token_code == REL_OP_T)
		{
			/*if attribute integer is not the same as parameter token attribute, it calls syn_eh()*/
			if (lookahead.attribute.get_int != pr_token_attribute)
			{
				syn_eh(pr_token_code);
				return;
			}
		}

		lookahead = malar_next_token();

		/*if token code is ERR_T, it calls syn_printe(), gets a next token, and increments error count*/
		if (lookahead.code == ERR_T)
		{
			syn_printe();
			lookahead = malar_next_token();
			synerrno++;
			return;
		}

	}
	else
	{
		/*token code is not the same as parameter token code*/
		syn_eh(pr_token_code);
		return;
	}

}

/*
* Purpose: This function handles panic mode error recovery
* Author: Soojin Han
* Versions: 1.0 2020-08-07
* Called Functions: syn_printe(),malar_next_token(),exit()
* Parameters: int sync_token_code
* Return value: void
* Algorithm: 1. it calls syn_printe()
			 2. it increments error count
			 3. it loops until it finds SEOF_T, or token code is different than parameter token code*/
void syn_eh(int sync_token_code)
{
	syn_printe();
	synerrno++;

	while (lookahead.code != sync_token_code)
	{
		lookahead = malar_next_token();

		/*if token code is the same as parameter code, it advances to the next token and checks if it's SEOF_T*/
		if (sync_token_code == lookahead.code)
		{
			lookahead = malar_next_token();

			/*token code is SEOF_T so it returns*/
			if (lookahead.code == SEOF_T)
			{
				return;
			}

			return;
		}

		/*if token code is SEOF_T, it exits with error messages*/
		if (lookahead.code == SEOF_T)
		{
			exit(synerrno);
			return;
		}

	}
}

/*
* Purpose: This function prints out error statements
* Author: Soojin Han
* Versions: 1.0 2020-08-07
* Called Functions: printf()
* Parameters: N/A
* Return value: void
* Algorithm: 1. it checks for matching token code and prints out it's statement */
void syn_printe()
{
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);

	switch (t.code) {

	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;
	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}

/*
* Purpose: This function takes an argument and prints it
* Author: Soojin Han
* Versions: 1.0 2020-08-07
* Called Functions: printf()
* Parameters: char* str
* Return value: void
* Algorithm: 1. prints str */
void gen_incode(char* str)
{
	printf("%s\n", str);
}

/*
* Purpose: This function runs grammar production functions
* Author: Soojin Han
* Versions: 1.0 2020-08-07
* Called Functions: match(),opt_statements(),gen_incode()
* Parameters: void
* Return value: void
* Algorithm: 1. it checks for matching KW_T and LBR_T
			 2. it calls opt_statements() and checks for matching RBR_T*/
void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();/* <opt_statements> -> <statements> | ϵ */
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}