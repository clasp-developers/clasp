/* scanner for Cando lisp language */

%{
#define	DEBUG_LEVEL_FULL

#include	<sstream>
#include	"foundation.h"
#include	"aggregate.h"
#include	"molecule.h"
#include	"residue.h"
#include	"atom.h"
#include	"stringList.h"
#include	"hold.h"
#include	"cons.h"

#include	"lispParse.tab.hh"


extern	void	lisperror(const string& msg);
extern	char	lispGetChar();
extern	istream*	lispStream;

#define	MAX_STR_CONST	256
char string_buf[MAX_STR_CONST];
char *string_buf_ptr;


int	
#define	isatty(x) (0)

#define	YY_INPUT(buf,result,max_size) \
	{ \
		if ( lispStream->eof() ) \
		{\
		    VP(("hit YY_NULL"));\
		    result = YY_NULL;\
		} else \
		{\
		    int c; \
		    c = lispGetChar();\
		    buf[0] = c;\
		    result = 1;\
		    VP(("YY_INPUT(%c/%d)",c,c));\
		}\
	}

#define	YY_USER_ACTION VP(("Recognized(%s)  matched rule: %d", lisptext, yy_act ));

void	lisp_lex_flush()
{
    YY_FLUSH_BUFFER;
}


// NAME must support  - and + in first character to
// support "+" and "-" operators, this may conflict with numbers

%}
%option noyywrap


DIGIT	[0-9]

KeyedName [a-zA-Z_][a-zA-Z0-9_]*[:]

NAME	[@\%\!\>\<\*\/:=a-zA-Z_][\.@=\%a-zA-Z0-9\+\-_]*


%x str


%%


<indent>" "	{currentLineIndent++; }
<indent>"\t"	{currentLineIndent = ( currentLineIndent + 8 ) & ~7; ); }
<indent>"\n"	{currentLineIndent = 0; /*Ignore blank lines*/ }
<indent>.	{	/* Anything other than whitespace */
		    unput(*yytext);
		    if ( currentLineIndent > currentIndentLevel()

"#"[^\n]*     /* eat up one-line comments */

[ \t\n]+          /* eat up whitespace */
";"	return lSemiColon;
"'"	return lQuoteChar;
"{"	return lOpenBrace;
"}"	return lCloseBrace;
"["	return lOpenPrefixForm;
"]"	return lClosePrefixForm;
"("	return lOpenPrefixForm;
")"	return lClosePrefixForm;
"+"	{
	    strcpy(lisplval.cval,lisptext);
	    return lSymbol;
	}
"-"	{
	    strcpy(lisplval.cval,lisptext);
	    return lSymbol;
	}
        

[+\-]?{DIGIT}+    {
		lisplval.ival = atoi(lisptext);
		return lInteger;
	    }

{KeyedName}	{ 
	    strcpy(lisplval.cval,lisptext);
	    return lKeyedName;
	}

[`]{NAME} { 
	    /* Any name prefixed with a back quote is a symbol.
	       This is to allow strings like "+xxx" to be accessed as
	       symbols
	    */
	    strcpy(lisplval.cval,&lisptext[1]);
	    return lSymbol;
	}
{NAME}   {
	    if ( strcmp( lisptext, "true") == 0 ) return lTrue;
	    if ( strcmp( lisptext, "false") == 0 ) return lFalse;
	    if ( strcmp( lisptext, "lambda") == 0 ) return lLambda;
	    if ( strcmp( lisptext, "if") == 0 ) return lIf;
	    if ( strcmp( lisptext, "cond") == 0 ) return lCond;
	    if ( strcmp( lisptext, "set") == 0 ) return lSetGlobal;
	    if ( strcmp( lisptext, ":=") == 0 ) return lSetGlobal;
	    if ( strcmp( lisptext, "let") == 0 ) return lSetLocal;
	    if ( strcmp( lisptext, "=") == 0 ) return lSetLocal;
	    if ( strcmp( lisptext, "defClass") == 0 ) return lDefineClass;
	    if ( strcmp( lisptext, "defFunction") == 0 ) return lDefineFunction;
	    if ( strcmp( lisptext, "defMethod") == 0 ) return lDefineMethod;
	    if ( strcmp( lisptext, "quote") == 0 ) return lQuote;
	    if ( strcmp( lisptext, "invoke") == 0 ) return lInvoke;
	    if ( strcmp( lisptext, "while") == 0 ) return lWhile;
	    if ( strcmp( lisptext, "foreach") == 0 ) return lForEach;
	    if ( strcmp( lisptext, "block") == 0 ) return lBlock;
	    if ( strcmp( lisptext, "blockDEBUG") == 0 ) return lBlockDebug;
	    if ( strcmp( lisptext, "LOG") == 0 ) return lLog;
	    if ( strcmp( lisptext, "ASSERT") == 0 ) return lAssert;
	    if ( strcmp( lisptext, "blockLOG") == 0 ) return lBlockLog;
	    if ( strcmp( lisptext, "return") == 0 ) return lReturn;
	    if ( strcmp( lisptext, "break") == 0 ) return lBreak;
	    if ( strcmp( lisptext, "continue") == 0 ) return lContinue;
	    if ( strcmp( lisptext, "raise") == 0 ) return lRaise;
	    strcpy(lisplval.cval,lisptext);
	    return lSymbol;
	}


[+\-]?{DIGIT}+"."{DIGIT}*        {
		lisplval.dval = atof(lisptext);
		return lDouble;
	     }

[+\-]?{DIGIT}+"."{DIGIT}*[eE][+\-]?{DIGIT}        {
		lisplval.dval = atof(lisptext);
		return lDouble;
	     }



\"      string_buf_ptr = string_buf; BEGIN(str);

<str>\"        { /* saw closing quote - all done */
	 BEGIN(INITIAL);
	 *string_buf_ptr = '\0';
	 strcpy(lisplval.cval,string_buf);
	 return lString;
	 }

<str>\n        {
	 /* error - unterminated string constant */
	 /* generate error message */
	 }

<str>\\[0-7]{1,3} {
	 /* octal escape sequence */
	 int result;

	 (void) sscanf( yytext + 1, "%o", &result );

	 if ( result > 0xff )
		 /* error, constant is out-of-bounds */

	 *string_buf_ptr++ = result;
	 }

<str>\\[0-9]+ {
	 /* generate error - bad escape sequence; something
	  * like '\48' or '\0777777'
	  */
	 }

<str>\\n  *string_buf_ptr++ = '\n';
<str>\\t  *string_buf_ptr++ = '\t';
<str>\\r  *string_buf_ptr++ = '\r';
<str>\\b  *string_buf_ptr++ = '\b';
<str>\\f  *string_buf_ptr++ = '\f';

<str>\\(.|\n)  *string_buf_ptr++ = yytext[1];

<str>[^\\\n\"]+        {
	 char *yptr = yytext;

	 while ( *yptr )
		 *string_buf_ptr++ = *yptr++;
	 }
[\000]	{ VP(("Hit a zero character, terminating")); yyterminate(); };
<<EOF>>	{ VP(("<<EOF>> hit, terminating ")); yyterminate(); };

.           {
    		string serr = lisptext;
		lisperror("Unknown input character("+serr+")");
	    }

%%

