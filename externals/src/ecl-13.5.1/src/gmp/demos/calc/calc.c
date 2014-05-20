/* A Bison parser, made from calc.y
   by GNU bison 1.34.  */

#define YYBISON 1  /* Identify Bison output.  */

# define	EOS	257
# define	BAD	258
# define	HELP	259
# define	HEX	260
# define	DECIMAL	261
# define	QUIT	262
# define	ABS	263
# define	BIN	264
# define	FIB	265
# define	GCD	266
# define	KRON	267
# define	LCM	268
# define	LUCNUM	269
# define	NEXTPRIME	270
# define	POWM	271
# define	ROOT	272
# define	SQRT	273
# define	NUMBER	274
# define	VARIABLE	275
# define	LOR	276
# define	LAND	277
# define	EQ	278
# define	NE	279
# define	LE	280
# define	GE	281
# define	LSHIFT	282
# define	RSHIFT	283
# define	UMINUS	284

#line 1 "calc.y"

/* A simple integer desk calculator using yacc and gmp.

Copyright 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA. */


/* This is a simple program, meant only to show one way to use GMP for this
   sort of thing.  There's few features, and error checking is minimal.
   Standard input is read, calc_help() below shows the inputs accepted.

   Expressions are evaluated as they're read.  If user defined functions
   were wanted it'd be necessary to build a parse tree like pexpr.c does, or
   a list of operations for a stack based evaluator.  That would also make
   it possible to detect and optimize evaluations "mod m" like pexpr.c does.

   A stack is used for intermediate values in the expression evaluation,
   separate from the yacc parser stack.  This is simple, makes error
   recovery easy, minimizes the junk around mpz calls in the rules, and
   saves initializing or clearing "mpz_t"s during a calculation.  A
   disadvantage though is that variables must be copied to the stack to be
   worked on.  A more sophisticated calculator or language system might be
   able to avoid that when executing a compiled or semi-compiled form.

   Avoiding repeated initializing and clearing of "mpz_t"s is important.  In
   this program the time spent parsing is obviously much greater than any
   possible saving from this, but a proper calculator or language should
   take some trouble over it.  Don't be surprised if an init/clear takes 3
   or more times as long as a 10 limb addition, depending on the system (see
   the mpz_init_realloc_clear example in tune/README).  */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gmp.h"
#define NO_CALC_H /* because it conflicts with normal calc.c stuff */
#include "calc-common.h"


#define numberof(x)  (sizeof (x) / sizeof ((x)[0]))


void
calc_help (void)
{
  printf ("Examples:\n");
  printf ("    2+3*4        expressions are evaluated\n");
  printf ("    x=5^6        variables a to z can be set and used\n");
  printf ("Operators:\n");
  printf ("    + - *        arithmetic\n");
  printf ("    / %%          division and remainder (rounding towards negative infinity)\n");
  printf ("    ^            exponentiation\n");
  printf ("    !            factorial\n");
  printf ("    << >>        left and right shifts\n");
  printf ("    <= >= >      \\ comparisons, giving 1 if true, 0 if false\n");
  printf ("    == != <      /\n");
  printf ("    && ||        logical and/or, giving 1 if true, 0 if false\n");
  printf ("Functions:\n");
  printf ("    abs(n)       absolute value\n");
  printf ("    bin(n,m)     binomial coefficient\n");
  printf ("    fib(n)       fibonacci number\n");
  printf ("    gcd(a,b,..)  greatest common divisor\n");
  printf ("    kron(a,b)    kronecker symbol\n");
  printf ("    lcm(a,b,..)  least common multiple\n");
  printf ("    lucnum(n)    lucas number\n");
  printf ("    nextprime(n) next prime after n\n");
  printf ("    powm(b,e,m)  modulo powering, b^e%%m\n");
  printf ("    root(n,r)    r-th root\n");
  printf ("    sqrt(n)      square root\n");
  printf ("Other:\n");
  printf ("    hex          \\ set hex or decimal for input and output\n");
  printf ("    decimal      /   (\"0x\" can be used for hex too)\n");
  printf ("    quit         exit program (EOF works too)\n");
  printf ("    ;            statements are separated with a ; or newline\n");
  printf ("    \\            continue expressions with \\ before newline\n");
  printf ("    # xxx        comments are # though to newline\n");
  printf ("Hex numbers must be entered in upper case, to distinguish them from the\n");
  printf ("variables a to f (like in bc).\n");
}


int  ibase = 0;
int  obase = 10;


/* The stack is a fixed size, which means there's a limit on the nesting
   allowed in expressions.  A more sophisticated program could let it grow
   dynamically.  */

mpz_t    stack[100];
mpz_ptr  sp = stack[0];

#define CHECK_OVERFLOW()                                                  \
  if (sp >= stack[numberof(stack)])                                       \
    {                                                                     \
      fprintf (stderr,                                                    \
               "Value stack overflow, too much nesting in expression\n"); \
      YYERROR;                                                            \
    }

#define CHECK_EMPTY()                                                   \
  if (sp != stack[0])                                                   \
    {                                                                   \
      fprintf (stderr, "Oops, expected the value stack to be empty\n"); \
      sp = stack[0];                                                    \
    }


mpz_t  variable[26];

#define CHECK_VARIABLE(var)                                             \
  if ((var) < 0 || (var) >= numberof (variable))                        \
    {                                                                   \
      fprintf (stderr, "Oops, bad variable somehow: %d\n", var);        \
      YYERROR;                                                          \
    }


#define CHECK_UI(name,z)                        \
  if (! mpz_fits_ulong_p (z))                   \
    {                                           \
      fprintf (stderr, "%s too big\n", name);   \
      YYERROR;                                  \
    }


#line 143 "calc.y"
#ifndef YYSTYPE
typedef union {
  char  *str;
  int   var;
} yystype;
# define YYSTYPE yystype
#endif
#ifndef YYDEBUG
# define YYDEBUG 0
#endif



#define	YYFINAL		118
#define	YYFLAG		-32768
#define	YYNTBASE	44

/* YYTRANSLATE(YYLEX) -- Bison token number corresponding to YYLEX. */
#define YYTRANSLATE(x) ((unsigned)(x) <= 284 ? yytranslate[x] : 50)

/* YYTRANSLATE[YYLEX] -- Bison token number corresponding to YYLEX. */
static const char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    39,     2,     2,     2,    36,     2,     2,
      41,    42,    34,    32,    43,    33,     2,    35,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      24,    40,    25,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    38,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    26,    27,
      28,    29,    30,    31,    37
};

#if YYDEBUG
static const short yyprhs[] =
{
       0,     0,     2,     5,     8,    12,    15,    16,    18,    22,
      24,    26,    28,    30,    34,    38,    42,    46,    50,    54,
      58,    62,    66,    69,    72,    76,    80,    84,    88,    92,
      96,   100,   104,   109,   116,   121,   126,   133,   138,   143,
     148,   157,   164,   169,   171,   173,   175,   179,   181
};
static const short yyrhs[] =
{
      46,     0,    45,    46,     0,    46,     3,     0,    45,    46,
       3,     0,     1,     3,     0,     0,    47,     0,    21,    40,
      47,     0,     5,     0,     6,     0,     7,     0,     8,     0,
      41,    47,    42,     0,    47,    32,    47,     0,    47,    33,
      47,     0,    47,    34,    47,     0,    47,    35,    47,     0,
      47,    36,    47,     0,    47,    38,    47,     0,    47,    30,
      47,     0,    47,    31,    47,     0,    47,    39,     0,    33,
      47,     0,    47,    24,    47,     0,    47,    28,    47,     0,
      47,    26,    47,     0,    47,    27,    47,     0,    47,    29,
      47,     0,    47,    25,    47,     0,    47,    23,    47,     0,
      47,    22,    47,     0,     9,    41,    47,    42,     0,    10,
      41,    47,    43,    47,    42,     0,    11,    41,    47,    42,
       0,    12,    41,    48,    42,     0,    13,    41,    47,    43,
      47,    42,     0,    14,    41,    49,    42,     0,    15,    41,
      47,    42,     0,    16,    41,    47,    42,     0,    17,    41,
      47,    43,    47,    43,    47,    42,     0,    18,    41,    47,
      43,    47,    42,     0,    19,    41,    47,    42,     0,    21,
       0,    20,     0,    47,     0,    48,    43,    47,     0,    47,
       0,    49,    43,    47,     0
};

#endif

#if YYDEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined. */
static const short yyrline[] =
{
       0,   167,   169,   171,   173,   174,   176,   178,   183,   189,
     190,   191,   192,   197,   199,   200,   201,   202,   203,   204,
     206,   208,   210,   212,   214,   215,   216,   217,   218,   219,
     221,   222,   224,   225,   227,   229,   230,   232,   233,   235,
     236,   237,   239,   241,   247,   257,   259,   261,   263
};
#endif


#if (YYDEBUG) || defined YYERROR_VERBOSE

/* YYTNAME[TOKEN_NUM] -- String name of the token TOKEN_NUM. */
static const char *const yytname[] =
{
  "$", "error", "$undefined.", "EOS", "BAD", "HELP", "HEX", "DECIMAL", 
  "QUIT", "ABS", "BIN", "FIB", "GCD", "KRON", "LCM", "LUCNUM", 
  "NEXTPRIME", "POWM", "ROOT", "SQRT", "NUMBER", "VARIABLE", "LOR", 
  "LAND", "'<'", "'>'", "EQ", "NE", "LE", "GE", "LSHIFT", "RSHIFT", "'+'", 
  "'-'", "'*'", "'/'", "'%'", "UMINUS", "'^'", "'!'", "'='", "'('", "')'", 
  "','", "top", "statements", "statement", "e", "gcdlist", "lcmlist", 0
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives. */
static const short yyr1[] =
{
       0,    44,    44,    45,    45,    45,    46,    46,    46,    46,
      46,    46,    46,    47,    47,    47,    47,    47,    47,    47,
      47,    47,    47,    47,    47,    47,    47,    47,    47,    47,
      47,    47,    47,    47,    47,    47,    47,    47,    47,    47,
      47,    47,    47,    47,    47,    48,    48,    49,    49
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN. */
static const short yyr2[] =
{
       0,     1,     2,     2,     3,     2,     0,     1,     3,     1,
       1,     1,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     3,     3,
       3,     3,     4,     6,     4,     4,     6,     4,     4,     4,
       8,     6,     4,     1,     1,     1,     3,     1,     3
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error. */
static const short yydefact[] =
{
       0,     0,     9,    10,    11,    12,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    44,    43,     0,
       0,     6,     1,     7,     5,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    43,    23,     0,
       2,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    22,     0,
       0,     0,    45,     0,     0,    47,     0,     0,     0,     0,
       0,     0,     8,    13,     4,    31,    30,    24,    29,    26,
      27,    25,    28,    20,    21,    14,    15,    16,    17,    18,
      19,    32,     0,    34,    35,     0,     0,    37,     0,    38,
      39,     0,     0,    42,     0,    46,     0,    48,     0,     0,
      33,    36,     0,    41,     0,    40,     0,     0,     0
};

static const short yydefgoto[] =
{
     116,    21,    22,    23,    63,    66
};

static const short yypact[] =
{
      39,    17,-32768,-32768,-32768,-32768,   -20,     0,     2,    25,
      28,    30,    33,    34,    37,    40,    46,-32768,   -18,   122,
     122,    89,    67,   462,-32768,   122,   122,   122,   122,   122,
     122,   122,   122,   122,   122,   122,   122,-32768,   -36,   252,
      87,-32768,   122,   122,   122,   122,   122,   122,   122,   122,
     122,   122,   122,   122,   122,   122,   122,   122,-32768,   273,
     142,   294,   462,   -38,   164,   462,   -24,   315,   336,   186,
     208,   357,   462,-32768,-32768,   479,   495,   511,   511,   511,
     511,   511,   511,    29,    29,    50,    50,   -36,   -36,   -36,
     -36,-32768,   122,-32768,-32768,   122,   122,-32768,   122,-32768,
  -32768,   122,   122,-32768,   378,   462,   399,   462,   230,   420,
  -32768,-32768,   122,-32768,   441,-32768,    91,    92,-32768
};

static const short yypgoto[] =
{
  -32768,-32768,    90,   -19,-32768,-32768
};


#define	YYLAST		550


static const short yytable[] =
{
      38,    39,    57,    58,    94,    95,    59,    60,    61,    62,
      64,    65,    67,    68,    69,    70,    71,    72,    97,    98,
      24,    25,    36,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -6,
       1,    26,    -6,    27,     2,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    52,    53,    54,    55,    56,    28,    57,    58,    29,
      41,    30,    19,   104,    31,    32,   105,   106,    33,   107,
      20,    34,   108,   109,    54,    55,    56,    35,    57,    58,
      74,   117,   118,   114,     2,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    19,     0,     0,     0,     0,     0,     0,     0,
      20,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    37,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    19,     0,     0,     0,     0,
       0,     0,     0,    20,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
      57,    58,     0,     0,     0,    92,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,    57,    58,     0,     0,     0,    96,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,    57,    58,     0,     0,     0,   101,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,    57,    58,     0,     0,
       0,   102,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,    57,    58,
       0,     0,     0,   112,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
      57,    58,     0,     0,    73,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,    57,    58,     0,     0,    91,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,    57,    58,     0,     0,    93,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,    57,    58,     0,     0,    99,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,    57,    58,     0,     0,   100,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,    57,    58,     0,     0,   103,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,    57,    58,     0,     0,
     110,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,    57,    58,     0,
       0,   111,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,    57,    58,
       0,     0,   113,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,    57,
      58,     0,     0,   115,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
      57,    58,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,    57,    58,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,    57,    58,-32768,-32768,-32768,-32768,-32768,
  -32768,    50,    51,    52,    53,    54,    55,    56,     0,    57,
      58
};

static const short yycheck[] =
{
      19,    20,    38,    39,    42,    43,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    42,    43,
       3,    41,    40,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,     0,
       1,    41,     3,    41,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    32,    33,    34,    35,    36,    41,    38,    39,    41,
       3,    41,    33,    92,    41,    41,    95,    96,    41,    98,
      41,    41,   101,   102,    34,    35,    36,    41,    38,    39,
       3,     0,     0,   112,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      41,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    33,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    41,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    -1,    -1,    -1,    43,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    -1,    -1,    -1,    43,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    -1,    38,    39,    -1,    -1,    -1,    43,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    -1,    -1,
      -1,    43,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      -1,    -1,    -1,    43,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    -1,    -1,    42,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      -1,    38,    39,    -1,    -1,    42,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    -1,    -1,    42,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    -1,    38,    39,    -1,    -1,    42,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    -1,    38,    39,    -1,    -1,    42,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    -1,    -1,    42,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    -1,    -1,
      42,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    -1,
      -1,    42,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      -1,    -1,    42,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    -1,    -1,    42,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    -1,    38,    39,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    -1,    38,
      39
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"

/* Skeleton output parser for bison,

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software
   Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser when
   the %semantic_parser declaration is not specified in the grammar.
   It was written by Richard Stallman by simplifying the hairy parser
   used when %semantic_parser is specified.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

#if ! defined (yyoverflow) || defined (YYERROR_VERBOSE)

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
# if YYLSP_NEEDED
  YYLTYPE yyls;
# endif
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# if YYLSP_NEEDED
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE) + sizeof (YYLTYPE))	\
      + 2 * YYSTACK_GAP_MAX)
# else
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)
# endif

/* Relocate the TYPE STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Type, Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	yymemcpy ((char *) yyptr, (char *) (Stack),			\
		  yysize * (YYSIZE_T) sizeof (Type));			\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (Type) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif /* ! defined (yyoverflow) || defined (YYERROR_VERBOSE) */


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).

   When YYLLOC_DEFAULT is run, CURRENT is set the location of the
   first token.  By default, to implement support for ranges, extend
   its range to the last symbol.  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)       	\
   Current.last_line   = Rhs[N].last_line;	\
   Current.last_column = Rhs[N].last_column;
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#if YYPURE
# if YYLSP_NEEDED
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, &yylloc, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval, &yylloc)
#  endif
# else /* !YYLSP_NEEDED */
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval)
#  endif
# endif /* !YYLSP_NEEDED */
#else /* !YYPURE */
# define YYLEX			yylex ()
#endif /* !YYPURE */


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif

#if ! defined (yyoverflow) && ! defined (yymemcpy)
# if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#  define yymemcpy __builtin_memcpy
# else				/* not GNU C or C++ */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
#  if defined (__STDC__) || defined (__cplusplus)
yymemcpy (char *yyto, const char *yyfrom, YYSIZE_T yycount)
#  else
yymemcpy (yyto, yyfrom, yycount)
     char *yyto;
     const char *yyfrom;
     YYSIZE_T yycount;
#  endif
{
  register const char *yyf = yyfrom;
  register char *yyt = yyto;
  register YYSIZE_T yyi = yycount;

  while (yyi-- != 0)
    *yyt++ = *yyf++;
}
# endif
#endif

#ifdef YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif
#endif

#line 319 "/usr/share/bison/bison.simple"


/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif

/* YY_DECL_VARIABLES -- depending whether we use a pure parser,
   variables are global, or local to YYPARSE.  */

#define YY_DECL_NON_LSP_VARIABLES			\
/* The lookahead symbol.  */				\
int yychar;						\
							\
/* The semantic value of the lookahead symbol. */	\
YYSTYPE yylval;						\
							\
/* Number of parse errors so far.  */			\
int yynerrs;

#if YYLSP_NEEDED
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES			\
						\
/* Location data for the lookahead symbol.  */	\
YYLTYPE yylloc;
#else
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES
#endif


/* If nonreentrant, generate the variables here. */

#if !YYPURE
YY_DECL_VARIABLES
#endif  /* !YYPURE */

int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  /* If reentrant, generate the variables here. */
#if YYPURE
  YY_DECL_VARIABLES
#endif  /* !YYPURE */

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack. */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;

#if YYLSP_NEEDED
  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
#endif

#if YYLSP_NEEDED
# define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
# define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  YYSIZE_T yystacksize = YYINITDEPTH;


  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
#if YYLSP_NEEDED
  YYLTYPE yyloc;
#endif

  /* When reducing, the number of symbols on the RHS of the reduced
     rule. */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
#if YYLSP_NEEDED
  yylsp = yyls;
#endif
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  */
# if YYLSP_NEEDED
	YYLTYPE *yyls1 = yyls;
	/* This used to be a conditional around just the two extra args,
	   but that might be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
# else
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);
# endif
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (short, yyss);
	YYSTACK_RELOCATE (YYSTYPE, yyvs);
# if YYLSP_NEEDED
	YYSTACK_RELOCATE (YYLTYPE, yyls);
# endif
# undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
#if YYLSP_NEEDED
      yylsp = yyls + yysize - 1;
#endif

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG
     /* We have to keep this `#if YYDEBUG', since we use variables
	which are defined only if `YYDEBUG' is set.  */
      if (yydebug)
	{
	  YYFPRINTF (stderr, "Next token is %d (%s",
		     yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise
	     meaning of a token, for further debugging info.  */
# ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
# endif
	  YYFPRINTF (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to the semantic value of
     the lookahead token.  This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

#if YYLSP_NEEDED
  /* Similarly for the default location.  Let the user run additional
     commands if for instance locations are ranges.  */
  yyloc = yylsp[1-yylen];
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
#endif

#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] > 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif

  switch (yyn) {

case 5:
#line 174 "calc.y"
{ sp = stack[0]; yyerrok; }
    break;
case 7:
#line 178 "calc.y"
{
      mpz_out_str (stdout, obase, sp); putchar ('\n');
      sp--;
      CHECK_EMPTY ();
    }
    break;
case 8:
#line 183 "calc.y"
{
      CHECK_VARIABLE (yyvsp[-2].var);
      mpz_swap (variable[yyvsp[-2].var], sp);
      sp--;
      CHECK_EMPTY ();
    }
    break;
case 9:
#line 189 "calc.y"
{ calc_help (); }
    break;
case 10:
#line 190 "calc.y"
{ ibase = 16; obase = -16; }
    break;
case 11:
#line 191 "calc.y"
{ ibase = 0;  obase = 10; }
    break;
case 12:
#line 192 "calc.y"
{ exit (0); }
    break;
case 14:
#line 199 "calc.y"
{ sp--; mpz_add    (sp, sp, sp+1); }
    break;
case 15:
#line 200 "calc.y"
{ sp--; mpz_sub    (sp, sp, sp+1); }
    break;
case 16:
#line 201 "calc.y"
{ sp--; mpz_mul    (sp, sp, sp+1); }
    break;
case 17:
#line 202 "calc.y"
{ sp--; mpz_fdiv_q (sp, sp, sp+1); }
    break;
case 18:
#line 203 "calc.y"
{ sp--; mpz_fdiv_r (sp, sp, sp+1); }
    break;
case 19:
#line 204 "calc.y"
{ CHECK_UI ("Exponent", sp);
                     sp--; mpz_pow_ui (sp, sp, mpz_get_ui (sp+1)); }
    break;
case 20:
#line 206 "calc.y"
{ CHECK_UI ("Shift count", sp);
                     sp--; mpz_mul_2exp (sp, sp, mpz_get_ui (sp+1)); }
    break;
case 21:
#line 208 "calc.y"
{ CHECK_UI ("Shift count", sp);
                     sp--; mpz_fdiv_q_2exp (sp, sp, mpz_get_ui (sp+1)); }
    break;
case 22:
#line 210 "calc.y"
{ CHECK_UI ("Factorial", sp);
                     mpz_fac_ui (sp, mpz_get_ui (sp)); }
    break;
case 23:
#line 212 "calc.y"
{ mpz_neg (sp, sp); }
    break;
case 24:
#line 214 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_cmp (sp, sp+1) <  0); }
    break;
case 25:
#line 215 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_cmp (sp, sp+1) <= 0); }
    break;
case 26:
#line 216 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_cmp (sp, sp+1) == 0); }
    break;
case 27:
#line 217 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_cmp (sp, sp+1) != 0); }
    break;
case 28:
#line 218 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_cmp (sp, sp+1) >= 0); }
    break;
case 29:
#line 219 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_cmp (sp, sp+1) >  0); }
    break;
case 30:
#line 221 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_sgn (sp) && mpz_sgn (sp+1)); }
    break;
case 31:
#line 222 "calc.y"
{ sp--; mpz_set_ui (sp, mpz_sgn (sp) || mpz_sgn (sp+1)); }
    break;
case 32:
#line 224 "calc.y"
{ mpz_abs (sp, sp); }
    break;
case 33:
#line 225 "calc.y"
{ sp--; CHECK_UI ("Binomial base", sp+1);
                                    mpz_bin_ui (sp, sp, mpz_get_ui (sp+1)); }
    break;
case 34:
#line 227 "calc.y"
{ CHECK_UI ("Fibonacci", sp);
                                    mpz_fib_ui (sp, mpz_get_ui (sp)); }
    break;
case 36:
#line 230 "calc.y"
{ sp--; mpz_set_si (sp,
                                          mpz_kronecker (sp, sp+1)); }
    break;
case 38:
#line 233 "calc.y"
{ CHECK_UI ("Lucas number", sp);
                                    mpz_lucnum_ui (sp, mpz_get_ui (sp)); }
    break;
case 39:
#line 235 "calc.y"
{ mpz_nextprime (sp, sp); }
    break;
case 40:
#line 236 "calc.y"
{ sp -= 2; mpz_powm (sp, sp, sp+1, sp+2); }
    break;
case 41:
#line 237 "calc.y"
{ sp--; CHECK_UI ("Nth-root", sp+1);
                                    mpz_root (sp, sp, mpz_get_ui (sp+1)); }
    break;
case 42:
#line 239 "calc.y"
{ mpz_sqrt (sp, sp); }
    break;
case 43:
#line 241 "calc.y"
{
        sp++;
        CHECK_OVERFLOW ();
        CHECK_VARIABLE (yyvsp[0].var);
        mpz_set (sp, variable[yyvsp[0].var]);
      }
    break;
case 44:
#line 247 "calc.y"
{
        sp++;
        CHECK_OVERFLOW ();
        if (mpz_set_str (sp, yyvsp[0].str, ibase) != 0)
          {
            fprintf (stderr, "Invalid number: %s\n", yyvsp[0].str);
            YYERROR;
          }
      }
    break;
case 46:
#line 259 "calc.y"
{ sp--; mpz_gcd (sp, sp, sp+1); }
    break;
case 48:
#line 263 "calc.y"
{ sp--; mpz_lcm (sp, sp, sp+1); }
    break;
}

#line 705 "/usr/share/bison/bison.simple"


  yyvsp -= yylen;
  yyssp -= yylen;
#if YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;
#if YYLSP_NEEDED
  *++yylsp = yyloc;
#endif

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[YYTRANSLATE (yychar)]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[YYTRANSLATE (yychar)]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* defined (YYERROR_VERBOSE) */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*--------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action |
`--------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;
      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;


/*-------------------------------------------------------------------.
| yyerrdefault -- current state does not do anything special for the |
| error token.                                                       |
`-------------------------------------------------------------------*/
yyerrdefault:
#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */

  /* If its default is to accept any token, ok.  Otherwise pop it.  */
  yyn = yydefact[yystate];
  if (yyn)
    goto yydefault;
#endif


/*---------------------------------------------------------------.
| yyerrpop -- pop the current state because it cannot handle the |
| error token                                                    |
`---------------------------------------------------------------*/
yyerrpop:
  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#if YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "Error: state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

/*--------------.
| yyerrhandle.  |
`--------------*/
yyerrhandle:
  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

/*---------------------------------------------.
| yyoverflowab -- parser overflow comes here.  |
`---------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}
#line 265 "calc.y"


yyerror (char *s)
{
  fprintf (stderr, "%s\n", s);
}

int calc_option_readline = -1;

int
main (int argc, char *argv[])
{
  int  i;

  for (i = 1; i < argc; i++)
    {
      if (strcmp (argv[i], "--readline") == 0)
        calc_option_readline = 1;
      else if (strcmp (argv[i], "--noreadline") == 0)
        calc_option_readline = 0;
      else if (strcmp (argv[i], "--help") == 0)
        {
          printf ("Usage: calc [--option]...\n");
          printf ("  --readline    use readline\n");
          printf ("  --noreadline  don't use readline\n");
          printf ("  --help        this message\n");
          printf ("Readline is only available when compiled in,\n");
          printf ("and in that case it's the default on a tty.\n");
          exit (0);
        }
      else
        {
          fprintf (stderr, "Unrecognised option: %s\n", argv[i]);
          exit (1);
        }
    }

#if WITH_READLINE
  calc_init_readline ();
#else
  if (calc_option_readline == 1)
    {
      fprintf (stderr, "Readline support not available\n");
      exit (1);
    }
#endif

  for (i = 0; i < numberof (variable); i++)
    mpz_init (variable[i]);

  for (i = 0; i < numberof (stack); i++)
    mpz_init (stack[i]);

  return yyparse ();
}
