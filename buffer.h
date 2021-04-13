/*	File name: buffer.h
	Compiler: MS Visual Studio 2019
	Author: Soojin Han, 040698591
	Course: CST 8152 – Compilers, Lab Section: 012
	Assignment: 3
	Date: August 7, 2020
	Professor: Paulo Sousa
	Purpose: To understand the structure of buffer system and command line arguments to send output with buffer operational mode.
	Function list: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_addcoffset(), b_capacity(), b_markc(), b_mode(), b_incfactor(), 
	b_load(), b_isempty(), b_getc), b_eob(), b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(),
	b_location() */

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)				/* operation failure return value -1 */
#define RT_FAIL_2 (-2)				/* operation failure return value -2 */
#define LOAD_FAIL (-2)				/* load fail return value -2*/
#define DEFAULT_INIT_CAPACITY 200	/* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */

/* You should add your own constant definitions here */

#define MAX_CAPACITY (SHRT_MAX-1)   /* MAXIMUM ALLOWED POSITIVE VALUE – 1  */
#define ADDITIVE_SELF_INCREMENTING_MODE 1	/* additive mode 1  */
#define MULTIPLICATIVE_SELF_INCREMENTING_MODE (-1)	/* multiplcative mode -1  */
#define FIXED_MODE 0				/* fixed mode 0  */
#define RESET 0						/* reset to zero  */
#define EMPTY 0						/* buffer is empty  */
#define RUNTIME_ERROR 256			/* runtime error value 256 */
#define BUFFER_NOT_END 9			/* end of buffer not reached value 9 */
#define BUFFER_FULL 1				/* end of buffer reached value 1 */
#define BUFFER_NOT_FULL 0			/* buffer is not full */
#define RT_SUCCESS 1				/* operation success return value 1 */
#define RT_ON_FULL 0				/* operation full return value 0 */
#define RW_SUCCESS 0				/* operation rewind success return value 0 */

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFF9
#define SET_EOB 0x0002 
#define RESET_EOB 0XFFFD
#define CHECK_EOB 0x0002
#define SET_R_FLAG 0X0004
#define RESET_R_FLAG 0XFFFB
#define CHECK_R_FLAG 0X0004

/* user data type declarations */
typedef struct BufferDescriptor {

	char* cb_head;	/* pointer to the beginning of character array (character buffer) */
	short capacity;	/* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;	/* the offset (in chars) to the add-character location */
	short getc_offset;	/* the offset (in chars) to the get-character location */
	short markc_offset;	/* the offset (in chars) to the mark location */
	char inc_factor;	/* character array increment factor*/
	char mode;	/* operational mode indicator*/
	unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, * pBuffer;

/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer* const pBD);
void b_free(Buffer* const pBD);
int b_isfull(Buffer* const pBD);
short b_addcoffset(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
short b_markc(pBuffer const pBD, short mark);
int b_mode(Buffer* const pBD);
size_t b_incfactor(Buffer* const pBD);
int b_load(FILE* const fi, Buffer* const pBD);
int b_isempty(Buffer* const pBD);
char b_getc(Buffer* const pBD);
int b_eob(Buffer* const pBD);
int b_print(Buffer* const pBD, char nl);
Buffer* b_compact(Buffer* const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_reset(Buffer* const pBD);
short b_getcoffset(Buffer* const pBD);
int b_rewind(Buffer* const pBD);
char* b_location(Buffer* const pBD, short loc_offset);

#endif

