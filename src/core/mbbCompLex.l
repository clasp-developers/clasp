
%option	yylineno
%option noyywrap

/* scanner for a CyclomerDatabase definition */


/*
 *
 * Cyclomer database lexical analyser
 *
 */


%{
/* need this for the call to atof() below */
#include <math.h>
#include	<stdio.h>
#if 0
#define	LEX_DEBUG	1
#endif
#if 0
#define	LEX_STRIP	1
#endif

#include	<string.h>
#include	"bond.h"
#include	"mbbCompGrammar.tab.h"

%}

DIGIT    [0-9]
ID       [a-z][a-z0-9]*

%%



		/* Integers */
[-]?{DIGIT}+	{
#ifdef	LEX_DEBUG
    printf( "An integer: %s (%d)\n", yytext, atoi( yytext ) );
#endif
    yylval.ival = atoi(yytext);
    return T_INTEGER;
}

		/* Floats */
[-]?{DIGIT}+"."{DIGIT}*        |
[-]?{DIGIT}+[eE][-+]{DIGIT}+ |
[-]?{DIGIT}+"."{DIGIT}*[eE][-+]{DIGIT}+        {
#ifdef	LEX_DEBUG
    printf( "A float: %s (%g)\n", yytext, atof( yytext ) );
#endif
    yylval.dval = atof(yytext);
    return T_FLOAT;
}

"'"[^']*"'"	{
#ifdef	LEX_DEBUG
    printf( "A string: %s\n", yytext );
#endif
#if 0
    strcpy( yylval.str, yytext );
#else
    strcpy( yylval.str, &yytext[1] );
    yylval.str[strlen(&yytext[1])-1] = '\0';
#endif
    return T_STRING;
}

"("	{ return T_OPENPAREN; }
")"	{ return T_CLOSEPAREN; }
"@"	{ return T_AT; }
"{"	{ return T_OPENBLOCK; }
"}"	{ return T_CLOSEBLOCK; }
";"	{ return T_SEMICOLON; }
"*"	{ return T_STAR; }
"backwardTransform"	{ return T_BACKWARDTRANSFORM; }
"center"		{ return T_CENTER; }
"conformation"		{ return T_CONFORMATION; }
"derivativeBonds"	{ return T_DERIVATIVEBONDS; }
"double"		{ return T_DOUBLE; }
"triple"		{ return T_TRIPLE; }
"forwardTransform"	{ return T_FORWARDTRANSFORM; }
"comment"		{ return T_COMMENT; }
"head"			{ return T_HEAD; }
"headAxes"		{ return T_HEADAXES; }
"headTransform"		{ return T_HEADTRANSFORM; }
"inLink"		{ return T_INLINK; }
"outLink"		{ return T_OUTLINK; }
"positions"		{ return T_POSITIONS; }
"single"		{ return T_SINGLE; }
"start"			{ return T_START; }
"stop"			{ return T_STOP; }
"tail"			{ return T_TAIL; }
"tailAxes"		{ return T_TAILAXES; }
"tailTransform"		{ return T_TAILTRANSFORM; }
"terminator"		{ return T_TERMINATOR; }
"cyclomer"		{ return T_CYCLOMER; }
"residue"		{ return T_RESIDUE; }
"atom"			{ return T_ATOM; }
"bond"			{ return T_BOND; }
"parameterSet"		{ return T_PARAMETERSET; }
"type"			{ return T_TYPE; }
"angle"			{ return T_ANGLE; }
"torsion"		{ return T_TORSION; }
"improper"		{ return T_IMPROPER; }
"nonBond"		{ return T_NONBOND; }


"//".*"\n"	/* eat up one-line comments */
[ \t\n\r]+	/* eat up whitespace */ 

.		{ 
	yylval.str[0] = yytext[0];
	yylval.str[1] = '\0';
	return T_CHAR;
}

%%


