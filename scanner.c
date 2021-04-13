/*	File name: scanner.c
	Compiler: MS Visual Studio 2019
	Author: Soojin Han, 040698591
	Course: CST 8152 – Compilers, Lab Section: 012
	Assignment: 3
	Date: August 7, 2020
	Professor: Paulo Sousa
	Purpose: To provide functions to implement Lexical Analyzer (Scanner)
	Function list: char_class(), get_next_state(), iskeyword(), scanner_init(), malar_next_token(),
	aa_func02(), aa_func03(), aa_func08(),aa_func05(),aa_func10(), aa_func11(), aa_func12()	 */

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

	 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"  /*to implement buffer functions*/
#include "token.h"	 /*to implement token functions*/
#include "table.h"	 /*to implement token functions*/

#define DEBUG  /* for conditional processing  */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum; /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf; /*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(pBuffer psc_buf)
{
	if (b_isempty(psc_buf)) return EXIT_FAILURE; /*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS; /*0*/
	/*   scerrnum = 0;  */ /*no need - global ANSI C */
}

/*
* Purpose: This function determines the token state and return the token
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: b_getc(),isspace(),isalnum(),b_markc(),b_getcoffset(),get_next_state(),b_retract(),
					b_allocate(),strcpy(),b_reset(),b_compact(),b_location(),b_free(),b_addc(),aa_func11(),
					aa_func12()
* Parameters: None
* Return value: Token
* Algorithm: 1. if c is a white-space character and it goes to the next character
			 2. if c is SEOF or 255, it assigns token code to SEOF_T and return the token
			 3. if c is either an alphabet or a quotation mark, it calls b_markc() method to assign
				lextstart point, calls get_next_state() to assign the next state, and checks as_table
				to assign state status
			 4. if state status is NOAS, it calls b_getc() to get the next character, and assign state,
				stateStatus, and tempChar, and it loops until the stateStatus is not NOAS(not accepting state)
			 5. if stateSatus is ASWR(accepting state with return), the buffer is retracted
			 6. lexend is assigned by b_getcoffset()
			 8. it allocates memory for lex_buff
			 9. if the alloction failed, it assigns token code to ERR_T and set token attribute err_lex to
				runtime error
			 10. it resets sc_buf to add the characters to lex_buf
			 11. lex_buff is compacted with the terminator character and token t is assigned from aa_table
			 12. it frees lex_buf and returns token t
			 13. if c is not a letter or quotation mark, it goes to the symbol switch loop and assigns a 
			     token according to each symbol */
Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart; /*start offset of a lexeme in the input char buffer (array) */
	short lexend; /*end   offset of a lexeme in the input char buffer (array)*/
	int stateStatus = NOAS; /*token status to be used and initialized to not accepting state */
	char tempChar = -1; /*temporary character to be used to store each character obtained from the buffer*/
	short stringLine = 0;

	/*endless loop that is broken by token */
	while (1)
	{
		c = b_getc(sc_buf);

		/*  filters  '\t', '\v', '\r', '\n',' ', '\f'  */
		if (isspace(c))
		{
			if (c == '\n')
			{
				line++;
			}

			continue;
		}

		/*loop is broken by end of file token*/
		if (c == SEOF || c == 255)
		{
			t.code = SEOF_T;
			t.attribute.seof = 1;
			return t;
		}

		/*detecting alphanumeric or a quotation*/
		if (isalnum(c) != 0 || c == '"')
		{
			/*starting mark from the buffer*/
			lexstart = b_markc(sc_buf, b_getcoffset(sc_buf) - 1);
			/*token state*/
			state = get_next_state(state, c);
			/*accepting state*/
			stateStatus = as_table[state];

			if (c=='"')
			{
				/*it loops until state changes to accepting state*/
				while (stateStatus == NOAS)
				{
					c = b_getc(sc_buf); /*gets a char from the buffer*/

					if (c=='\n')
					{
						/*new line present in string literal*/
						stringLine++;
					}

					if (c==SEOF)
					{
						/*source end of file reached while reading in string literal*/
						line += stringLine;	
					}

					state = get_next_state(state, c);
					stateStatus = as_table[state];
				}
			}
			else
			{
				while (stateStatus == NOAS)
				{
					c = b_getc(sc_buf); /*gets a char from the buffer*/
					state = get_next_state(state, c);
					stateStatus = as_table[state];
				}
			}		

			/*if state is accepting state with return, buffer decrements getc_offset by 1*/
			if (stateStatus == ASWR)
			{
				b_retract(sc_buf);
			}

			lexend = b_getcoffset(sc_buf);

			/*allocating memory for a buffer*/
			lex_buf = b_allocate(lexend - lexstart, 0, 'f');

			/*allocation failed*/
			if (!lex_buf)
			{
				t.code = ERR_T;
				scerrnum = 1;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				return t;
			}

			/*resetting buffer getc_offset to markc_offset*/
			b_reset(sc_buf);

			/*adding characters to lex_buf*/
			while (b_getcoffset(sc_buf) < lexend)
			{
				c = b_getc(sc_buf);
				b_addc(lex_buf, c);
			}
			

			b_compact(lex_buf, '\0');
			/*creating a token from accepting function callback table*/
			t = aa_table[state](b_location(lex_buf, 0));
			b_free(lex_buf);
			return t;
		}

		switch (c)
		{
		case ';': t.code = EOS_T; /* End of statement */
			return t;
		case '#':
			c = b_getc(sc_buf);

			/*determining if it's a string concatenation*/
			if (c == '#')
			{
				/* strong concatenation token */
				t.code = SCC_OP_T;
				return t;
			}
			else
			{
				b_retract(sc_buf);
				t = aa_func12("#");
				return t;
			}

		case '{':
			t.code = LBR_T; /* Left brace */
			return t;
		case '}':
			t.code = RBR_T; /* Right brace */
			return t;
		case '+':
			t.code = ART_OP_T; /* Addition operator */
			t.attribute.arr_op = PLUS;
			return t;
		case '-':
			t.code = ART_OP_T; /* Subtraction operator */
			t.attribute.arr_op = MINUS;
			return t;
		case '!':
			tempChar = b_getc(sc_buf);
			c = tempChar;

			/* it loops until new line is found*/
			while (c != '\n')
			{
				c = b_getc(sc_buf);

				/* end of file found */
				if (c == SEOF || c == 255)
				{
					t.code = SEOF_T;
					return t;
				}
			}

			line++;

			/*comment token found*/
			if (tempChar == '!')
			{
				continue;
			}
			else
			{
				/* error state */
				b_retract(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = tempChar;
				t.attribute.err_lex[2] = '\0';
				return t;
			}

		case '(':
			t.code = LPR_T; /* Left parenthesis */
			return t;
		case ')':
			t.code = RPR_T; /* Right parenthesis */
			return t;
		case '*':
			t.code = ART_OP_T; /* Multiplication operator */
			t.attribute.arr_op = MULT;
			return t;
		case '/':
			t.code = ART_OP_T; /* Division operator */
			t.attribute.arr_op = DIV;
			return t;
		case '.':
			b_markc(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);

			switch (c)
			{
			case 'A':

				/*AND logical operator found*/
				if (b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.')
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;
				}

			case 'O':

				/*OR logical operator found*/
				if (b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;
				}

			default: /*Error token*/
				b_reset(sc_buf);
				t = aa_func11(".");
				return t;
			}

		case '>':
			t.code = REL_OP_T; /* Greater than relational operator */
			t.attribute.rel_op = GT;
			return t;
		case ',':
			t.code = COM_T; /* Comma */
			return t;
		case '<':
			c = b_getc(sc_buf);

			if (c != '>')
			{
				/*Less than operator*/
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
			}
			else
			{
				/*Not equal operator*/
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
			}

			return t;
		case '=':
			c = b_getc(sc_buf);

			/*assignment operator*/
			if (c != '=')
			{
				b_retract(sc_buf);
				t.code = ASS_OP_T;
			}
			else
			{
				/*equal to operator*/
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}

			return t;
		default:
			/* error token */
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';
			return t;
		}
	}
}

/*
* Purpose: This function determines the next state of the token
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: char_class(),assert()
* Parameters: int state, char c
* Return value: int
* Algorithm: 1. it gets column index from char_class function
			 2. it finds the state from the st_table */
int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*
* Purpose: This function determines the column index for the transition table
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: isalpha(),isdigit()
* Parameters: char c
* Return value: int
* Algorithm: 1. if c is alphabet, it returns 0.
			 2. if c is 0, it returns 1.
			 3. if c is a digit, it returns 2.
			 4. if c is . , it returns 3.
			 5. if c is #, it returns 4.
			 6. if c is quotation, it returns 6.
			 7. if c is end of file character, it returns 7.
			 8. otherwise it returns 5.	 */
int char_class(char c)
{
	int option;

	/*assigning column number according to the status of c */
	if (isalpha(c))
	{
		option = 0; /*c is alphabet */
	}
	else if (c == '0')
	{
		option = 1; /*c is 0 */
	}
	else if (isdigit(c))
	{
		option = 2; /*c is a digit */
	}
	else if (c == '.')
	{
		option = 3; /*c is a period */
	}
	else if (c == '#')
	{
		option = 4; /*c is # */
	}
	else if (c == '"')
	{
		option = 6; /*c is a quotation */
	}
	else if (c == SEOF || c == 255)
	{
		option = 7; /*c is end of file */
	}
	else
	{
		option = 5;/* anything else is set as 5 */
	}

	return option;
}

/*
* Purpose: This function implements arithmentic variable identifier AND keywords(VID- AVID/ KW)
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: iskeyword(),strlen(),strcpy()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. if token is not a keyword, token attribute vid_lex is copied with lexeme
			 2. if token is a keyword, token code is set to KW_T and kwt_idx is set to the index
				of the kw_table*/
Token aa_func02(char lexeme[])
{
	Token token;
	const int tokenL = iskeyword(lexeme);
	int i = 0;

	/*token is not a keyword*/
	if (tokenL == -1)
	{
		/*token is within range of variable identifier length*/
		if (strlen(lexeme) <= VID_LEN)
		{
			strcpy(token.attribute.vid_lex, lexeme);
		}
		else
		{
			/* storing the first 8 letters to the token attribute */
			while (i < VID_LEN)
			{
				token.attribute.vid_lex[i] = lexeme[i];
				i++;
			}

			token.attribute.vid_lex[VID_LEN] = '\0';
		}

		token.code = AVID_T; /*assigning token code to Arithmetic Variable identifier token*/
	}
	else
	{
		/*token is a keyword*/
		token.code = KW_T;
		token.attribute.kwt_idx = tokenL;
		return token;
	}

	return token;
}

/*
* Purpose: This function implements string variable identifier(VID- SVID)
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: strncpy(),strlen()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. if length of lexeme is less than or equal to VID_LEN, token attribute vid_lex is
				copied with lexeme
			 2. otherwise toke attribute vid_lex is copied with the first seven characters of
				lexeme followed by # */
Token aa_func03(char lexeme[])
{
	Token token;
	const unsigned int length = strlen(lexeme);

	token.code = SVID_T; /*assigning String Variable identifier token */

	/* the lexeme length less than or equal to variable identifier length */
	if (length <= VID_LEN)
	{
		token.attribute.err_lex[length] = '\0';
		strncpy(token.attribute.vid_lex, lexeme, length);
	}
	else
	{
		/*the lexeme length is greater than variable identifier length,
		 *and copying the first seven characters of lexeme to the
		 *token attribute */
		strncpy(token.attribute.vid_lex, lexeme, 7);

		/* Add the # character*/
		token.attribute.vid_lex[7] = '#';
		token.attribute.vid_lex[VID_LEN] = '\0';
	}

	return token;
}

/*
* Purpose: This function implements integer literal and decimal constant(DIL) token
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: atol()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. if number lexeme is greater than SHRT_MAX,or less than SHRT_MIN, it goes to aa_table12
			 3. otherwise, token attribute int value is set to the tokenNum and code is INL_T */
Token aa_func05(char lexeme[])
{
	Token token;
	const long tokenNum = atol(lexeme);

	if (tokenNum > SHRT_MAX || tokenNum < SHRT_MIN)
	{
		/*token length is greater than maximum allowed or less than minimum allowed so it goes to
		  function aa_table12*/
		token = aa_table[12](lexeme);
	}
	else
	{
		/*it sets token attribute int_value to tokenNum and token code to integer literal*/
		token.attribute.int_value = tokenNum;
		token.code = INL_T;
	}

	return token;
}

/*
* Purpose: This function implements floating point literal token
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: strtof(),strlen()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. if float lexeme is negative, it goes to aa_table12
			 2. if float lexeme is less than float min or greater than float max,
				and length of lexeme is greater than 7, it goes to aa_table12
			 3. otherwise, it sets the token code to FPL_T and attribute to tokenNum */
Token aa_func08(char lexeme[])
{
	const float tokenNum = strtof(lexeme, NULL);
	Token token = { 0 };

	if (tokenNum < 0 || ((tokenNum < FLT_MIN || tokenNum > FLT_MAX) && (tokenNum >= 0 && strlen(lexeme) > 7)))
	{
		/*token is not a valid floating point literal token*/
		token = aa_table[12](lexeme);
	}
	else
	{
		/*token is a floating point literal*/
		token.code = FPL_T;
		token.attribute.flt_value = tokenNum;
	}

	return token;
}

/*
* Purpose: This function implements string literal token
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: b_addcoffset(), strlen(), b_addc()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. it loops through the string lexeme and add each character to str_LTBL
			 2. It adds character terminator symbol at the end */
Token aa_func10(char lexeme[])
{
	Token t = { 0 };
	unsigned int i = 0;	

	t.code = STR_T;/*token is string literal*/
	t.attribute.str_offset = b_addcoffset(str_LTBL);

	/*adding lexeme to str_LTBL buffer*/
	for (i = 1; i < strlen(lexeme) - 1; i++)
	{
		/*it increases line number when there is a new line symbol*/
		if (lexeme[i] == '\n')
		{
			line++;
		}

		if (lexeme[i]==SEOF || lexeme[i] == 255)
		{
			t=aa_func12(lexeme);
			return t;
		}

		b_addc(str_LTBL, lexeme[i]);
	}

	/*adding terminator character to the buffer*/
	b_addc(str_LTBL, '\0');
	return t;
}

/*
* Purpose: This function implements an error state with no retract
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: strcpy(), strlen()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. if token is less than or equal to the valid error length, it copies the string to the roken
				error attribute
			 2. if token is greater than the valid error length, it copies the first 17 letters of the string
				to the token attribute.*/
Token aa_func11(char lexeme[])
{
	Token t;
	unsigned int tokenL = strlen(lexeme);

	t.code = ERR_T;/*error token*/

	if (tokenL <= ERR_LEN)
	{
		/*copying lexeme to token attribute error*/
		strcpy(t.attribute.err_lex, lexeme);
		t.attribute.err_lex[tokenL] = '\0';
	}
	else
	{
		/*lexeme length is greater than error message length*/
		strncpy(t.attribute.err_lex, lexeme, 17);
		t.attribute.err_lex[17] = '.';
		t.attribute.err_lex[18] = '.';
		t.attribute.err_lex[19] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	return t;
}

/*
* Purpose: This function implements an error state with retract
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: strcpy(), strlen()
* Parameters: char lexeme[]
* Return value: token
* Algorithm: 1. if token is less than or equal to the valid error length, it copies the string to the roken
				error attribute
			 2. if token is greater than the valid error length, it copies the first 17 letters of the string
				to the token attribute.*/
Token aa_func12(char lexeme[])
{
	Token t;
	unsigned int tokenL = strlen(lexeme);

	t.code = ERR_T;/*error token*/

	if (tokenL <= ERR_LEN)
	{
		/*copying lexeme to token attribute error*/
		strcpy(t.attribute.err_lex, lexeme);
		t.attribute.err_lex[tokenL] = '\0';
	}
	else
	{
		/*lexeme length is greater than error message length*/
		strncpy(t.attribute.err_lex, lexeme, 17);
		t.attribute.err_lex[17] = '.';
		t.attribute.err_lex[18] = '.';
		t.attribute.err_lex[19] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}

	return t;
}

/*
* Purpose: This function determines if kw_lexeme is one of the keywords
* Author: Soojin Han
* Versions: 1.0 2020-07-17
* Called Functions: strcmp()
* Parameters: char* kw_lexeme
* Return value: int, -1
* Algorithm: 1. check if the string is null
			 2. if string is null, it returns -1
			 3. go through kw_table to find the string, and returns the string index.
			 4.	returns -1 if the string is not found*/
int iskeyword(char* kw_lexeme)
{
	int count = 0;

	/* kw_lexeme is not a keyword so it returns -1 */
	if (!kw_lexeme)
	{
		return -1;
	}

	/* looping through the kw_table to find the matching keyword */
	while (count < KWT_SIZE)
	{
		if (strcmp(kw_table[count], kw_lexeme) == 0)
		{
			//it returns the index if the match is found
			return count;
		}

		count++;
	}

	return -1;
}
