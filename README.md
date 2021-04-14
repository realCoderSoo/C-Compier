# C-Compiler
The program involves the PLATYPUS Parser implementation. Part 1 involves Implementing a PLATYPUS Parser. Part 2 includes some rudimentary semantic analysis, writing a PLATYPUS cross-compiler, or writing a PLATYPUS interpreter.

In order to build a RDPP you need to modify the syntactical part of the PLATYPUS
Grammar (The Platypus Syntactic Specification). The grammar provided for you in
PlatypusLGR_S20.pdf is an LR grammar (that is, a grammar suitable for LR parsing).
You must transform it into an LL grammar suitable for Recursive Descent Predictive
Parsing. To accomplish that you should follow the steps outlined bellow.
1. Check the PLATYPUS Grammar for completeness and correctness.
2. Eliminate the left recursion and apply left factoring where needed.
Some of the syntactic productions must be rewritten to make them suitable for recursive
decent predictive parsing. Do not forget that our grammar (PlatypusLGR_S20.pdf) is
an LR grammar, which must be transformed into an equivalent LL grammar. For
example, the productions of the type
<statements> -> <statement> | <statements> <statement>
can be rearranged in a convenient for transformation form
<statements> -> <statements> <statement> | <statement>
  
Write your parser() function.
void parser(void){
lookahead = malar_next_token();
program(); match(SEOF_T,NO_ATTR);
gen_incode("PLATY: Source file parsed");
}
Step 3:
Write your match() function. The prototype for the function is:
void match(int pr_token_code,int pr_token_attribute);
The match() function matches two tokens: the current input token (lookahead) and the
token required by the parser. The token required by the parser is represented by two
integers - the token code (pr_token_code), and the token attribute
(pr_token_attribute). The attribute code is used only when the token code is one of
the following codes: KW_T, LOG_OP_T, ART_OP_T, REL_OP_T. In all other cases
the token code is matched only.
If the match is successful and the lookahead is SEOF_T, the function returns.
If the match is successful and the lookahead is not SEOF_T, the function advances to
the next input token by executing the statement:

lookahead = malar_next_token();
If the new lookahead token is ERR_T, the function calls the error printing function
syn_printe(), advances to the next input token by calling malar_next_token () again,
increments the error counter synerrno, and returns.
If the match is unsuccessful, the function calls the error handler
syn_eh(pr_token_code) and returns.
Note: Make your match() function as efficient as possible. This function is called many
times during the parsing. The function will be graded with respect to design and
efficiency.
Step 4:
Write the error handling function syn_eh(). This function implements a simple panic
mode error recovery.
void syn_eh(int sync_token_code)
First, the function calls syn_printe() and increments the error counter. Then the
function implements a panic mode error recovery: the function advances the input token
(lookahead) until it finds a token code matching the one required by the parser
(pr_token_code passed to the function as sync_token_code ).
It is possible, when advancing, that the function can reach the end of the source file
without finding the matching token. To prevent from overrunning the input buffer, before
every move the function checks if the end of the file is reached. If the function looks for
sync_token_code different from SEOF_T and reaches the end of the source file, the
function calls exit(synerrno).
If a matching token is found and the matching token is not SEOF_T, the function
advances the input token one more time and returns. If a matching token is found and
the matching token is SEOF_T, the function returns.

Step 5:
Write the error printing function syn_printe() .
void syn_printe()
Note: This function implementation is provided for you in Assignment3MPTF_S20.zip.
The function prints the following error message:
PLATY: Syntax error: Line: line_number_of_the_syntax_error
***** Token code:lookahead token code Attribute: token attribute
and returns. For example:
PLATY: Syntax error: Line: 2
***** Token code: 13 Attribute: NA
PLATY: Syntax error: Line: 8

***** Token code: 9 Attribute: 0
PLATY: Syntax error: Line: 9
***** Token code: 2 Attribute: sum
PLATY: Syntax error: Line: 11
***** Token code: 4 Attribute: 0.5
PLATY: Syntax error: Line: 17
***** Token code: 6 Attribute: Result:
PLATY: Syntax error: Line: 21
***** Token code: 16 Attribute: ELSE
If the offending token is a keyword, variable identifier or string literal you must use the
corresponding token attribute to access and print the lexeme (keyword name, variable
name, or string).
For example, to print the keyword lexeme you must use the kw_table defined in
table.h. Important note: You are not allowed to copy the keyword table in parser.h or
parser.c. You must use a proper declaration to create an external link to the one
defined in table.h.
Similarly, you must use the string literal table to print the sting literals.
Step 6:
Write the gen_incode() function. In Part 1 of this assignment the function takes a string
as an argument and prints it. Later the function can be modified and used to emit
intermediate (Bonus 1) or machine code. The function may be called any time a
production is recognized (see parser()). The format of the message is: “PLATY:
Program parsed”, “PLATY: Assignment statement parsed”, and so on (see the sample
WRITE files).
Step 7:
For each of your grammar productions write a function named after the name of the
production. For example:
void program(void){
match(KW_T,PLATYPUS);match(LBR_T,NO_ATTR);opt_statements();
match(RBR_T,NO_ATTR);
gen_incode("PLATY: Program parsed");
}
Writing a production function, follow the sub steps below.
Step 7.1:
To implement the Parser, you must use the modified grammar (see Task 1). Before
writing a function, analyze carefully the production. If the production consists of a single
production rule (no alternatives), write the corresponding function without using the
FIRST set (see above). If you use the lookahead to verify in advance whether to
proceed with the production and call the syn_printe() function, your WRITE might
report quite different syntax errors than my parser will reports.

Example: The production:
<input statement> ->
READ (<variable list>);
MUST be implemented as follows:
void input_statement(void){
match(KW_T,READ);match(LPR_T,NO_ATTR);variable_list();
match(RPR_T,NO_ATTR); match(EOS_T,NO_ATTR);
gen_incode("PLATY: Input statement parsed");
}
AND MUST NOT be implemented as shown below:
void input_statement(void){
if(lookahead.code == KW_T
&& lookahead.attribute.get_int== READ) {
match(KW_T,READ);match(LPR_T,NO_ATTR);variable_list();
match(RPR_T,NO_ATTR); match(EOS_T,NO_ATTR);
gen_incode("PLATY: Input statement parsed");
}else
syn_printe();
}
This implementation will “catch” the syntax error but will prevent the match() function
from calling the error handler at the right place.
Step 7.2:
If a production has more than one alternatives on the right side (even if one of them is
empty), you must use the FIRST set for the production.
For example, the FIRST set for the <opt_statements> production is: {KW_T(IF),
KW_T(WHILE) , KW_T(READ), KW_T(WRITE), AVID_T, SVID_T, and .
Here is an example how the FIRST set is used to write a function for a production:
/* FIRST(<opt_statements>)={AVID_T,SVID_T,KW_T(see above),e} */
void opt_statements(){
switch(lookahead.code){
case AVID_T:
case SVID_T: statements();break;
case KW_T:
/* check for IF,WHILE,READ,WRITE in statements_p() as well*/
if (lookahead.attribute.get_int == IF
|| lookahead.attribute.get_int == WHILE
|| lookahead.attribute.get_int == READ
|| lookahead.attribute.get_int == WRITE){
statements();
break;
}
default: /*empty string – optional statements*/ ;
gen_incode("PLATY: Opt_statements parsed");
}
}
Pay special attention to the implementation of the empty string. If you do not have an
empty string in your production, you must call the syn_printe() function at that point.
