/*	File name: buffer.c
	Compiler: MS Visual Studio 2019
	Author: Soojin Han, 040698591
	Course: CST 8152 – Compilers, Lab Section: 012
	Assignment: 3
	Date: August 7, 2020
	Professor: Paulo Sousa
	Purpose: To understand the structure of buffer system and command line arguments to send output with buffer operational mode.
	Function list: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_addcoffset(), b_capacity(), b_markc(), b_mode(), b_incfactor(),
	b_load(), b_isempty(), b_getc), b_eob(), b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(),
	b_location()*/

#include "buffer.h"

/*
* Purpose: This function allocates memory for a new buffer.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: calloc(), malloc(), free()
* Parameters: short init_capacity, char inc_factor, char o_mode
* Return value: pBD, NULL
* Algorithm: 1. allocate memory for one Buffer with calloc()
			 2. allocates memory for one dynamic character buffer with malloc()
			 3. sets the Buffer structure operational mode indicator mode and the inc_factor.
			 4.	copies the given init_capacity value into the Buffer structure capacity variable;
			 5. sets the flags field to its default value which is FFF9 hexadecimal.*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {

	/* checks if capacity is negative or greater than the maxumum number */
	if (init_capacity < 0 || init_capacity>MAX_CAPACITY)
	{
		return NULL;
	}

	/*allocating one buffer and initializes it to zero*/
	Buffer* buffer = (Buffer*)calloc(1, sizeof(Buffer));
	buffer->cb_head = NULL;

	/* checks if buffer is valid, and returns NULL if it's not valid */
	if (!buffer)
	{
		return NULL;
	}

	/* if init_capacity is zero it tries to create a character buffer with default size 200 characters. 
	If the init_capacity is 0, the function ignores the current value of the parameter inc_factor and sets 
	the buffer structure inc_factor to 15 in mode a and m or to 0 in mode f. The pointer returned by 
	malloc() is assigned to cb_head*/
	if (init_capacity == 0)
	{
		/*allicating character buffer with the default capacity*/
		buffer->cb_head = (char*)malloc(sizeof(char) * DEFAULT_INIT_CAPACITY);

		/* it returns NULL if the character buffer is not allocated successfully*/
		if (buffer->cb_head == NULL)
		{
			return NULL;
		}
		buffer->capacity = DEFAULT_INIT_CAPACITY;

		/* if the mode additive or multiplicative, buffer inc_factor will be set to default inc_factor and 
			mode will be set to 1 for additive and -1 for multiplicative*/
		if (o_mode == 'a' || o_mode == 'm')
		{
			buffer->inc_factor = DEFAULT_INC_FACTOR;
			inc_factor = DEFAULT_INC_FACTOR;
			buffer->mode = o_mode == 'a' ? 1 : -1;
		}
		/* if o_mode is fixed it will set buffer mode to 0 and inc_factor to 0*/
		else if (o_mode == 'f')
		{
			buffer->mode = 0;
			buffer->inc_factor = 0;
		}
		/* if the buffer doesn't fit to all other options, it will be freed and return NULL*/
		else
		{
			free(buffer);
			return NULL;
		}
	}
	/* if init_capacity is not zero, the character buffer will be allocated with the init_capacity*/
	else
	{
		/*allicating character buffer with the init_capacity*/
		buffer->cb_head = (char*)malloc(sizeof(char) * init_capacity);

		/* it returns NULL if the character buffer is not allocated successfully*/
		if (buffer->cb_head == NULL)
		{
			return NULL;
		}
		buffer->capacity = init_capacity;
	}

	/* If the inc_factor parameter is 0 and init_capacity is not 0 (see above), the mode and the buffer inc_factor are set to 0. */
	if (init_capacity != 0 && inc_factor == 0)
	{
		buffer->inc_factor = 0;
		buffer->mode = 0;
	}
	/* otherwise, it will set those two according to o_mode */
	else
	{
		/* the mode and the buffer inc_factor are set to number 0 */
		if (o_mode == 'f')
		{
			buffer->mode = 0;
			buffer->inc_factor = 0;
		}
		/* If the o_mode is a and inc_factor is in the range of 1 to 255 inclusive , the mode is set to number 1 and the buffer 
		inc_factor is set to the value of inc_factor.  */
		else if (o_mode == 'a')
		{
			buffer->inc_factor = inc_factor;
			buffer->mode = ADDITIVE_SELF_INCREMENTING_MODE;
		}
		/* If the o_mode is m and inc_factor is in the range of 1 to 100 inclusive, the mode is set to number -1 and the 
		inc_factor value is assigned to the buffer inc_factor*/
		else if (o_mode == 'm' && ((unsigned char)inc_factor > 0 && (unsigned char)inc_factor <= 100))
		{
			buffer->mode = MULTIPLICATIVE_SELF_INCREMENTING_MODE;
			buffer->inc_factor = inc_factor;
		}
		/*if the buffer doesn't fit all the other conditions, the character buffer will be freed, buffer will be freed and return NULL*/
		else
		{
			free(buffer->cb_head);
			free(buffer);
			return NULL;
		}
	}

	buffer->flags = DEFAULT_FLAGS; 

	return buffer;
}

/*
* Purpose: The function adds a symbol to the operational character buffer depending on it's mode
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: realloc()
* Parameters: pBuffer const pBD, char symbol
* Return value: pBD, NULL
* Algorithm: 1. resets the flags field r_flag bit to 0 and tries to add the character symbol to the
				character array of the given buffer pointed by pBD.
			 2. If the buffer is operational and it is not full, the symbol can be stored in the
				character buffer.
			 3. If the character buffer is already full, the function will try to resize the buffer
				by increasing the current capacity to a new capacity.
			 4.	If the operational mode is 0, the function returns NULL.
			 5. How the capacity is increased depends on the current operational mode of the buffer.*/
pBuffer b_addc(pBuffer const pBD, char symbol) {

	/* checks if buffer is valid, and returns NULL if it's not valid */
	if (!pBD)
	{
		return NULL;
	}

	short availableSpace = 0;/* available space for mode -1 */
	short newInc = 0;/*new increment for mode -1*/
	short newCap = 0;/* new capacity for mode -1 */
	char* temp = NULL;/* temporary character buffer */

	pBD->flags = pBD->flags & RESET_EOB;

	/*if the buffer capacity is greater than buffer addc_offset and its capacity is less or equal than the max capacity, it adds the symbol and returns the buffer */
	if (pBD->capacity > pBD->addc_offset && pBD->capacity <= MAX_CAPACITY)
	{
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}

	/* array capacity is full and less than max buffer capacity */
	if (pBD->capacity <= MAX_CAPACITY && b_isfull(pBD))
	{
		/*if mode is fixed, it returns NULL*/
		if (pBD->mode == FIXED_MODE)
		{
			return NULL;
		}
		/*if mode is additive, new capacity is produced*/
		if (pBD->mode == ADDITIVE_SELF_INCREMENTING_MODE)
		{
			newCap = pBD->capacity + (unsigned char)pBD->inc_factor;
			if (newCap > MAX_CAPACITY && newCap > 0)
			{
				newCap = MAX_CAPACITY;
			}
			/*if new capacity is less than zero, it returns NULL*/
			if (newCap < 0)
			{
				return NULL;
			}
		}
		/*mode is multiplicative*/
		if (pBD->mode == MULTIPLICATIVE_SELF_INCREMENTING_MODE)
		{
			/*if buffer capacity is greater than the max capacity or buffer addc_offset is equal to the max capacity, it returns NULL*/
			if (pBD->capacity > MAX_CAPACITY || pBD->addc_offset == MAX_CAPACITY)
			{
				return NULL;
			}

			availableSpace = MAX_CAPACITY - (pBD->capacity);
			newInc = (short)(availableSpace * (((double)pBD->inc_factor) / 100));
			newCap = pBD->capacity + newInc;
			/*if the new capacity is greater than the max capacity, returns NULL*/
			if (newCap > MAX_CAPACITY)
			{
				return NULL;
			}
			/*if the new capacity is less than the max capacity and equal to the addc_offset, the max capacity is assigned to the new capacity*/
			if (newCap == pBD->addc_offset && newCap < MAX_CAPACITY)
			{
				newCap = MAX_CAPACITY;
			}
		}

		pBD->capacity = newCap;

		/*allocating memory for the temorary character buffer with the new capacity*/
		temp = (char*)realloc(pBD->cb_head, sizeof(char) * newCap);
		/*if the allocation fails, it returns NULL*/
		if (!temp)
		{
			return NULL;
		}
		pBD->flags = pBD->flags & RESET_R_FLAG;
		pBD->cb_head = temp;

		pBD->cb_head[pBD->addc_offset++] = symbol;

		return pBD;
	}

	return NULL;
}

/*
* Purpose: This function retains the memory space currently allocated to the buffer, but re-initializes all
		   appropriate data members of the given Buffer structure
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_SUCCESS, RT_FAIL_1
* Algorithm: 1. If buffer is NULL, it returns NULL
			 2. Resets addc_offset, flags, getc_offset, marc_offset to zero
			 3. Returns one if the operation was successful */
int b_clear(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}

	pBD->addc_offset = RESET;
	pBD->flags = DEFAULT_FLAGS;
	pBD->getc_offset = RESET;
	pBD->markc_offset = RESET;

	/* checks if buffer is valid after the modification, and returns NULL if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	else
	{
		return RT_SUCCESS;
	}
}

/*
* Purpose: The function de-allocates (frees) the memory occupied by the character buffer and the
		   Buffer structure (buffer descriptor).
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: Nothing
* Algorithm: 1. It frees up the memory for the character buffer and the buffer */
void b_free(Buffer* const pBD)
{
	/* checks if buffer is valid, and frees the character buffer and buffer */
	if (pBD)
	{
		free(pBD->cb_head);
		free(pBD);
	}
}

/*
* Purpose: The function determines if character buffer is full.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, BUFFER_FULL, RT_ON_FULL
* Algorithm: 1. The function returns 1 if the character buffer is full; it returns 0 otherwise.
				If a run-time error is possible, the function should return -1. */

int b_isfull(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}

	/*if character buffer capacity is equal to the max capacity, it returns BUFFER_FULL, and 
	returns zero otherwise*/
	if (pBD->capacity == pBD->addc_offset)
	{
		return BUFFER_FULL;
	}
	else
	{
		return BUFFER_NOT_FULL;
	}
}


/*
* Purpose: The function returns the current addc_offset.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->addc_offset
* Algorithm: 1. The function returns the current addc_offset. If a run-time error is possible,
				the function should return -1. */
short b_addcoffset(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	return pBD->addc_offset;
}

/*
* Purpose: The function returns the current capacity of the character buffer.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->capacity
* Algorithm: 1. The function returns the current capacity of the character buffer.
				If a run-time error is possible, the function should return -1. */
short b_capacity(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	return pBD->capacity;
}

/*
* Purpose: The function sets markc_offset to mark.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: pBuffer const pBD, short mark
* Return value: RT_FAIL_1, pBD->markc_offset
* Algorithm: 1. The parameter mark must be within the current limit of the buffer
				(0 to addc_offset inclusive).
			 2.	If a run-time error is possible, the function should return -1.
			 3. The function returns the currently set markc_offset */
short b_markc(pBuffer const pBD, short mark)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	/*if mark is less than addc_offset and mark is greater than zero, marc_offset is mark, else it returns RT_FAIL_1*/
	if (mark < (pBD->addc_offset) && mark >= 0)
	{
		pBD->markc_offset = mark;
	}
	else
	{
		return RT_FAIL_1;
	}

	return pBD->markc_offset;
}

/*
* Purpose: The function returns the value of mode to the calling function.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->mode
* Algorithm: 1. If a run-time error is possible, the function should notify the calling
				function about the failure .
			 2.	The function returns the value of mode to the calling function. */
int b_mode(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	return pBD->mode;
}

/*
* Purpose: The function returns the non-negative value of inc_factor to the calling function.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RUNTIME_ERROR,(size_t)(unsigned char)pBD->inc_factor
* Algorithm: 1. If a run-time error is possible, the function should return RUNTIME_ERROR.
			 2.	The function returns the non-negative value of inc_factor */
size_t b_incfactor(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RUNTIME_ERROR if it's not valid */
	if (!pBD)
	{
		return RUNTIME_ERROR;
	}

	return (size_t)(unsigned char)pBD->inc_factor;
}

/*
* Purpose: The function loads an open input file specified by fi into a buffer specified by pBD.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: fgetc(), feof(), b_addc(), ungetc()
* Parameters: FILE* const fi, Buffer* const pBD
* Return value: RT_FAIL_1, LOAD_FAIL, numOfChar
* Algorithm: 1. The function must use the standard function fgetc(fi) to read one character at a
				time and the function b_addc() to add the character to the buffer.
			 2.	If the current character cannot be added to the buffer (b_addc() returns NULL),
				the function returns the character to the file stream (file buffer) using ungetc()
				library function and then returns -2 (use the defined LOAD_FAIL constant).
			 3. The end-of-file character must not be added to the content of the buffer.
			 4. Only the standard macro feof(fi) must be used to detect end-of-file on the input file.
			 5. If some other run-time errors are possible, the function should return –1. If the
				loading operation is successful, the function must return the number of characters
				added to the buffer.*/
int b_load(FILE* const fi, Buffer* const pBD)
{
	int numReached = 0;
	char num = 0; /*checks if end of file is reached*/
	int numOfChar = 0;

	/* checks if buffer or file is valid and returns RT_FAIL_1 if it's not valid */
	if (!fi || !pBD)
	{
		return RT_FAIL_1;
	}
	/*this loop will get a character and checks if it reached end of file, and adds the character 
	to the buffer if it's not full*/
	do
	{
		num = (char)fgetc(fi);
		/*if it reached the end of file, it breaks out of the loop and returns number of characters*/
		if (feof(fi)) {
			numReached = 1;/*end of file reached*/
			break;
		}
		/*it adds the character to the buffer as long as it's not full, and it return LOAD_FAIL if 
		it is full*/
		if (b_addc(pBD, num) == NULL)
		{
			numReached = 1;
			ungetc(num, fi);
			return LOAD_FAIL;
		}
		
		numOfChar++;
	} while (numReached != 1);

	return numOfChar;
}

/*
* Purpose: The function determines if the buffer is empty.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, RT_SUCCESS, RT_ON_FULL
* Algorithm: 1. If the addc_offset is 0, the function returns 1; otherwise it returns 0. 
			    If a run-time error is possible, it should return -1.*/
int b_isempty(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	/*if addc_offset is zero it returns RT_SUCCESS*/
	if (pBD->addc_offset == EMPTY)
	{
		return RT_SUCCESS;
	}
	return 0;
}

/*
* Purpose: The function reads the buffer
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_2, RT_ON_FULL, pBD->cb_head[pBD->getc_offset++]
* Algorithm: 1. checks the argument for validity (possible run-time error). If it is not valid, 
			    it returns -2;
			 2.	if getc_offset and addc_offset are equal, using a bitwise operation it sets 
				the flags field eob bit to 1 and returns number 0; otherwise, using a bitwise 
				operation it sets eob to 0
			 3. returns the character located at getc_offset. Before returning it increments 
				getc_offset by 1.*/
char b_getc(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_2 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_2;
	}
	/*if getc_offset is greater than addc_offset it returns RT_FAIL_2 due to overrunning*/
	if (pBD->getc_offset > pBD->addc_offset)
	{
		return RT_FAIL_2;
	}
	/*it checks if the buffer reached the end of its content and returns RT_ON_FULL and
	sets the EOB flag if it reached the end*/
	if (pBD->addc_offset == pBD->getc_offset)
	{
		pBD->flags = pBD->flags | SET_EOB;
		return RT_ON_FULL;
	}
	/*it resets the EOB flag if the end of content of the buffer is not reached*/
	else
	{
		pBD->flags = pBD->flags & RESET_EOB;
	}

	return pBD->cb_head[pBD->getc_offset++];
}

/*
* Purpose: The function returns the value of the flags field 
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->flags |= SET_EOB, pBD->flags & CHECK_EOB
* Algorithm: 1. Bitwise operaiton is used to get the flags field;
			 2.	If a run-time error is possible, it should return -1.*/
int b_eob(Buffer* const pBD)
{
	/* checks if buffer is valid, and returns RT_FAIL_1 if it's not valid */
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	/*it checks if it reached the enf of buffer and sets the flag if it reached the end*/
	if (pBD->getc_offset == pBD->addc_offset)
	{
		return pBD->flags |= SET_EOB;
	}

	return pBD->flags & CHECK_EOB;
}

/*
* Purpose: The function prints character by character the contents of the character buffer to 
		   the standard output
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: b_getc(), b_eob()
* Parameters: Buffer* const pBD, char nl
* Return value: RT_FAIL_1, inc
* Algorithm: 1. In the loop, the function gets a character and checks if it's the end of buffer 
			 2.	After the loop ends, it checks the nl and if it is not 0, it prints a new line character. 
				Finally, it returns the number of characters printed. 
			 3. The function returns -1 on failure. */
int b_print(Buffer* const pBD, char nl)
{
	int inc = 0;
	int eob = 0;
	char getChar;

	/* checks if buffer or character buffer is valid, and return RT_FAIL_1 if it's not valid */
	if (!pBD || !pBD->cb_head)
	{
		return RT_FAIL_1;
	}
	/*this loop runs until it reaches the end of buffer*/
	while (!eob)
	{
		getChar = b_getc(pBD);
		if (!getChar)
		{
			printf("%c", getChar);
		}
		/*if the character is valid it prints the character*/
		if (getChar)
		{
			inc++;
			printf("%c", getChar);
		}

		eob = b_eob(pBD);
		/*if the end of buffer is reached, it breaks out of the loop*/
		if (eob)
		{
			break;
		}
	}
	/*if parameter nl is valid it prints out a new line*/
	if (nl)
	{
		printf("\n");
	}

	return inc;
}

/*
* Purpose: The function shrinks (or in some cases may expand) the buffer to a new capacity. 
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: realloc()
* Parameters: Buffer* const pBD, char symbol
* Return value: NULL, pBD
* Algorithm: 1. The new capacity is the current limit plus a space for one more character. 
			 2.	The function uses realloc() to adjust the new capacity, and then updates all 
				the necessary members of the buffer descriptor structure. 
			 3. Before returning a pointer to Buffer, the function adds the symbol to the end 
				of the character buffer and increments addc_offset.
			 4. The function must return NULL if for some reason it cannot to perform the required 
				operation. It must set the r_flag bit appropriately. */
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	short newCap = 0;
	char* newChar = NULL;

	/* checks if buffer or character buffer is valid, and return NULL if it's not valid */
	if (!pBD || !pBD->cb_head)
	{
		return NULL;
	}

	newCap = pBD->addc_offset + 1;
	/*if new capacity is less than or equal to zero, it returns NULL*/
	if (newCap <= EMPTY)
	{
		return NULL;
	}

	newChar = (char*)realloc(pBD->cb_head, (sizeof(char) * newCap));

	/*checks if character buffer is not valid, and returns NULL */
	if (!newChar)
	{
		return NULL;
	}

	pBD->flags &= RESET_R_FLAG;
	pBD->flags |= SET_EOB;
	pBD->cb_head = newChar;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->capacity = newCap;

	return pBD;
}

/*
* Purpose: The function returns the value of the flags field determined only by the r_flag bit.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, (char)(pBD->flags & CHECK_R_FLAG)
* Algorithm: 1. A bitwise operation is used to return the value of the flags field. 
			 2. If a run-time error is possible, it should return -1. */
char b_rflag(Buffer* const pBD)
{
	/*checks if buffer is valid, and returns RT_FAIL_1 if it's not valid*/
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	return (char)(pBD->flags & CHECK_R_FLAG);
}

/*
* Purpose: The function decrements getc_offset by 1. 
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->getc_offset--
* Algorithm: 1. It returns getc_offset.
			 2. If a run-time error is possible, it should return -1. */
short b_retract(Buffer* const pBD)
{
	/*checks if buffer is valid, and returns RT_FAIL_1 if it's not valid*/
	if (!pBD)
	{
		return RT_FAIL_1;
	}
	/*if retracting character is valid, getc_offset is decremented*/
	if (pBD->getc_offset > EMPTY)
	{
		return pBD->getc_offset--;
	}
	/*it returns RT_FAIL_1 if getc_offset is invalid*/
	else
	{
		return RT_FAIL_1;
	}
}

/*
* Purpose: The function sets getc_offset to the value of the current markc_offset. 
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->getc_offset
* Algorithm: 1. It returns getc_offset.
			 2. If a run-time error is possible, it should return -1. */
short b_reset(Buffer* const pBD)
{
	/*checks if buffer or marc_offset is valid, and returns RT_FAIL_1 if it's not valid*/
	if (!pBD || pBD->markc_offset < EMPTY)
	{
		return RT_FAIL_1;
	}

	pBD->getc_offset = pBD->markc_offset;

	return pBD->getc_offset;
}

/*
* Purpose: The function returns getc_offset to the calling function. 
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, pBD->getc_offset
* Algorithm: 1. It returns getc_offset.
			 2. If a run-time error is possible, it should return -1. */
short b_getcoffset(Buffer* const pBD)
{
	/*checks if buffer is valid, and returns RT_FAIL_1 if it's not valid*/
	if (!pBD)
	{
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/*
* Purpose: The function sets the getc_offset and markc_offset to 0, so that the buffer can be reread again.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD
* Return value: RT_FAIL_1, RW_SUCCESS
* Algorithm: 1. It sets the getc_offset and markc_offset to 0
			 2. If a run-time error is possible, it should return -1; otherwise it returns 0 */
int b_rewind(Buffer* const pBD)
{
	/*checks if buffer is valid, and returns RT_FAIL_1 if it's not valid*/
	if (!pBD)
	{
		return RT_FAIL_1;
	}

	pBD->getc_offset = RESET;
	pBD->markc_offset = RESET;

	return RW_SUCCESS;
}

/*
* Purpose: The function returns a pointer to the location of the character buffer indicated by loc_offset.
* Author: Soojin Han
* Versions: 1.0 2020-06-08
* Called Functions: Nothing
* Parameters: Buffer* const pBD, short loc_offset
* Return value: NULL, pBD->cb_head + loc_offset
* Algorithm: 1. It returns a pointer to the location of the character buffer indicated by loc_offset
			 2. If a run-time error is possible, it should return NULL */
char* b_location(Buffer* const pBD, short loc_offset)
{
	/*checks if buffer is valid, loc_offset is greater than zero, or loc_offset is greater or equal than addc_offset and returns RT_FAIL_1 if it's not valid*/
	if (!pBD || loc_offset < 0 || loc_offset >= pBD->addc_offset)
	{
		return NULL;
	}

	return pBD->cb_head + loc_offset;
}