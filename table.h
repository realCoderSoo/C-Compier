/*	File name: table.h
	Compiler: MS Visual Studio 2019
	Author: Soojin Han, 040698591
	Course: CST 8152 – Compilers, Lab Section: 012
	Assignment: 3
	Date: August 7, 2020
	Professor: Paulo Sousa
	Purpose: this header provides information about transition table, accepting state, accepting action function,
	accepting function callback table, and keywords table
	Function list: aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11(), aa_func12(),
	(*PTR_AAF)()    */
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define SEOF '\0' /*source end-of-file*/
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

	/* State transition table definition */
#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int st_table[][TABLE_COLUMNS] = {
	{1, 6, 4, ES, ES, ES, 9, ER},		/* State 0 */
	{1, 1, 1, 2, 3, 2, 2, ES},			/* State 1 */
	{IS, IS, IS, IS, IS, IS, IS, IS},	/* State 2 */
	{IS, IS, IS, IS, IS, IS, IS, IS},	/* State 3 */
	{5, 4, 4, 7, 5, 5, ER, ES},			/* State 4 */
	{IS, IS, IS, IS, IS, IS, IS, IS},	/* State 5 */
	{5, 6, ES, 7, ES, 5, ER, ES},		/* State 6 */
	{8, 7, 7, 8, 8, 8, 8, ES},			/* State 7 */
	{IS, IS, IS, IS, IS, IS, IS, IS},	/* State 8 */
	{9, 9, 9, 9, 9, 9, 10, ER},			/* State 9 */
	{IS, IS, IS, IS, IS,IS, IS, IS},	/* State 10 */
	{IS, IS, IS, IS, IS, IS, IS, IS},	/* State 11 */
	{IS, IS, IS, IS, IS, IS, IS, IS}	/* State 12 */
};

/* Accepting state table definition */
#define ASWR     0  /* accepting state with retract */
#define ASNR     1  /* accepting state with no retract */
#define NOAS     2  /* not accepting state */

int as_table[] = {
	NOAS, /* State 0 */
	NOAS, /* State 1 */
	ASWR, /* State 2 */
	ASNR, /* State 3 */
	NOAS, /* State 4 */
	ASWR, /* State 5 */
	NOAS, /* State 6 */
	NOAS, /* State 7 */
	ASWR, /* State 8 */
	NOAS, /* State 9 */
	ASNR, /* State 10 */
	ASNR, /* State 11 */
	ASWR  /* State 12 */
};

/* Accepting action function declarations */
Token aa_func02(char* lexeme); /* AVID/KW */
Token aa_func03(char* lexeme); /* SVID */
Token aa_func05(char* lexeme); /* DIL */
Token aa_func08(char* lexeme); /* FPL */
Token aa_func10(char* lexeme); /* SL  */
Token aa_func11(char* lexeme); /* ES  */
Token aa_func12(char* lexeme); /* ER  */

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/
typedef Token(*PTR_AAF)(char* lexeme);

/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[] = {
	NULL,		/* State 0 */
	NULL,		/* State 1 */
	aa_func02,  /* State 2 */
	aa_func03,  /* State 3 */
	NULL,		/* State 4 */
	aa_func05,  /* State 5 */
	NULL,		/* State 6 */
	NULL,		/* State 7 */
	aa_func08,  /* State 8 */
	NULL,		/* State 9 */
	aa_func10,  /* State 10 */
	aa_func11,	/* State 11 */
	aa_func12   /* State 12 */
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char* kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
