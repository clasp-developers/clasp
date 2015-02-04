/*
    File: msmartsParse.tab.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

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

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse smartsparse
#define yylex   smartslex
#define yyerror smartserror
#define yylval  smartslval
#define yychar  smartschar
#define yydebug smartsdebug
#define yynerrs smartsnerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     APEndOfLine = 258,
     APBond = 259,
     APAtomicNumber = 260,
     APChiralityAntiClockwise = 261,
     APChiralityClockwise = 262,
     APCloseBracket = 263,
     APCloseParenthesis = 264,
     APConnectivity = 265,
     APDegree = 266,
     APRingTest = 267,
     APResidueTest = 268,
     APDollarSign = 269,
     APDoubleBond = 270,
     APOrganicElement = 271,
     APInorganicElement = 272,
     APElectronegativeElement = 273,
     APError = 274,
     APGroupNumber = 275,
     APHeavyAtomTotalBond = 276,
     APImplicitHCount = 277,
     APLonePair = 278,
     APNegativeCharge = 279,
     APNegativeFormalCharge = 280,
     APNumber = 281,
     APOpenBracket = 282,
     APOpenParenthesis = 283,
     APOperatorAndHigh = 284,
     APOperatorAndLow = 285,
     APOperatorNot = 286,
     APOperatorOr = 287,
     APPiBondOrbital = 288,
     APAromaticPiElectron = 289,
     APPositiveCharge = 290,
     APPositiveFormalCharge = 291,
     APRingMembershipCount = 292,
     APRingSize = 293,
     APTotalHCount = 294,
     APTotalBondNumber = 295,
     APTotalExplicitBondNumber = 296,
     APTransitionMetal = 297,
     APValence = 298,
     APWildCard = 299
   };
#endif
/* Tokens.  */
#define APEndOfLine 258
#define APBond 259
#define APAtomicNumber 260
#define APChiralityAntiClockwise 261
#define APChiralityClockwise 262
#define APCloseBracket 263
#define APCloseParenthesis 264
#define APConnectivity 265
#define APDegree 266
#define APRingTest 267
#define APResidueTest 268
#define APDollarSign 269
#define APDoubleBond 270
#define APOrganicElement 271
#define APInorganicElement 272
#define APElectronegativeElement 273
#define APError 274
#define APGroupNumber 275
#define APHeavyAtomTotalBond 276
#define APImplicitHCount 277
#define APLonePair 278
#define APNegativeCharge 279
#define APNegativeFormalCharge 280
#define APNumber 281
#define APOpenBracket 282
#define APOpenParenthesis 283
#define APOperatorAndHigh 284
#define APOperatorAndLow 285
#define APOperatorNot 286
#define APOperatorOr 287
#define APPiBondOrbital 288
#define APAromaticPiElectron 289
#define APPositiveCharge 290
#define APPositiveFormalCharge 291
#define APRingMembershipCount 292
#define APRingSize 293
#define APTotalHCount 294
#define APTotalBondNumber 295
#define APTotalExplicitBondNumber 296
#define APTransitionMetal 297
#define APValence 298
#define APWildCard 299




/* Copy the first part of user declarations.  */
#line 70 "msmartsParse.yy"

#define DEBUG_LEVEL_FULL

#include<iostream>
#include<string>
#include<vector>
#include <istream>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <aggregate.h>
#include <molecule.h>
#include <residue.h>
#include <atom.h>

#include <chemInfo.h>

using namespace std;
using namespace core;

chemInfo::RPSmartsRoot  chemInfoTree;
map<string,int>	atomTagCounter;
stringstream	chemInfoErrorStream;

istream*	lexStream;		
// This defines the stream that we are reading from

#include <hold.h>

#define	LEXDEBUG	1

#ifdef	LEXDEBUG
#define	LEXPRINT(x) {LOG(BF("Token: %s") % x ); }
#define	LEXDPRINT(x) { LOG(BF("%s") % (x));}

#else
#define	LEXPRINT(x)
#define	LEXDPRINT(x)
#endif
#define	MAX_CVAL	16

RPLisp			msmartsEnv;

int	yylex();
void	yyerror(const char* str);



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union
#line 172 "msmartsParse.yy"
yyunion {
		const char*				eval;
		char					cval[MAX_CVAL];
		int 					ival;
		chemInfo::BondEnum             		benum;
    core::Hold<chemInfo::O_Root>*			root;
    core::Hold<chemInfo::O_SmartsRoot>*		smartsRoot;
    core::Hold<chemInfo::O_BondListMatchNode>*	bondListMatchNode;
    core::Hold<chemInfo::O_AtomOrBondMatchNode>*	atomOrBondMatchNode;
//		core::Hold<chemInfo::O_AtomTest>*		atomTest;
    core::Hold<chemInfo::O_BondTest>*		bondTest;
    core::Hold<chemInfo::O_Logical>*			logical;
	}
/* Line 193 of yacc.c.  */
#line 253 "msmartsParse.tab.cc"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 266 "msmartsParse.tab.cc"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  40
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   109

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  12
/* YYNRULES -- Number of rules.  */
#define YYNRULES  65
/* YYNRULES -- Number of states.  */
#define YYNSTATES  84

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   299

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     6,     8,    11,    14,    17,    19,
      21,    24,    26,    30,    32,    34,    37,    39,    43,    45,
      47,    50,    52,    55,    57,    60,    63,    66,    68,    71,
      73,    76,    78,    81,    83,    86,    89,    91,    93,    96,
      99,   101,   104,   108,   111,   113,   116,   120,   123,   126,
     128,   131,   133,   135,   137,   139,   141,   143,   146,   148,
     150,   153,   157,   161,   165,   170
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      46,     0,    -1,    47,    -1,    -1,    51,    -1,    51,    48,
      -1,    49,    48,    -1,    52,    48,    -1,    49,    -1,    52,
      -1,     4,    51,    -1,    51,    -1,    27,    54,     8,    -1,
      16,    -1,    55,    -1,    50,    26,    -1,    50,    -1,    28,
      48,     9,    -1,    44,    -1,    11,    -1,    11,    56,    -1,
      39,    -1,    39,    56,    -1,    22,    -1,    22,    56,    -1,
      12,    26,    -1,    13,    26,    -1,    37,    -1,    37,    56,
      -1,    38,    -1,    38,    56,    -1,    43,    -1,    43,    56,
      -1,    10,    -1,    10,    56,    -1,    20,    56,    -1,    18,
      -1,    21,    -1,    21,    56,    -1,    24,    56,    -1,    24,
      -1,    24,    24,    -1,    24,    24,    24,    -1,    35,    56,
      -1,    35,    -1,    35,    35,    -1,    35,    35,    35,    -1,
      42,    56,    -1,    34,    56,    -1,    33,    -1,     5,    56,
      -1,     6,    -1,     7,    -1,    56,    -1,    16,    -1,    17,
      -1,    23,    -1,    23,    56,    -1,    55,    -1,    53,    -1,
      31,    54,    -1,    54,    29,    54,    -1,    54,    32,    54,
      -1,    54,    30,    54,    -1,    14,    28,    47,     9,    -1,
      26,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   210,   210,   215,   216,   220,   227,   231,   235,   239,
     245,   249,   270,   274,   278,   287,   291,   296,   316,   327,
     332,   344,   349,   360,   365,   377,   388,   400,   404,   415,
     419,   428,   432,   442,   446,   452,   456,   460,   464,   477,
     481,   491,   500,   516,   520,   529,   538,   542,   546,   550,
     559,   563,   567,   576,   580,   584,   588,   592,   601,   678,
     688,   698,   708,   719,   726,   729
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "APEndOfLine", "APBond",
  "APAtomicNumber", "APChiralityAntiClockwise", "APChiralityClockwise",
  "APCloseBracket", "APCloseParenthesis", "APConnectivity", "APDegree",
  "APRingTest", "APResidueTest", "APDollarSign", "APDoubleBond",
  "APOrganicElement", "APInorganicElement", "APElectronegativeElement",
  "APError", "APGroupNumber", "APHeavyAtomTotalBond", "APImplicitHCount",
  "APLonePair", "APNegativeCharge", "APNegativeFormalCharge", "APNumber",
  "APOpenBracket", "APOpenParenthesis", "APOperatorAndHigh",
  "APOperatorAndLow", "APOperatorNot", "APOperatorOr", "APPiBondOrbital",
  "APAromaticPiElectron", "APPositiveCharge", "APPositiveFormalCharge",
  "APRingMembershipCount", "APRingSize", "APTotalHCount",
  "APTotalBondNumber", "APTotalExplicitBondNumber", "APTransitionMetal",
  "APValence", "APWildCard", "$accept", "input", "chemInfo", "chain",
  "bondAtomTest", "acyclicAtomTest", "atomTest", "branch",
  "atomPrimativeTest", "logOp", "recursiveChemInfo", "intNumber", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    45,    46,    47,    47,    47,    48,    48,    48,    48,
      49,    49,    50,    50,    50,    51,    51,    52,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    54,
      54,    54,    54,    54,    55,    56
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     0,     1,     2,     2,     2,     1,     1,
       2,     1,     3,     1,     1,     2,     1,     3,     1,     1,
       2,     1,     2,     1,     2,     2,     2,     1,     2,     1,
       2,     1,     2,     1,     2,     2,     1,     1,     2,     2,
       1,     2,     3,     2,     1,     2,     3,     2,     2,     1,
       2,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       2,     3,     3,     3,     4,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     0,    13,     0,     0,     2,    16,     4,    14,     3,
       0,    51,    52,    33,    19,     0,     0,    54,    55,    36,
       0,    37,    23,    56,    40,    65,     0,    49,     0,    44,
      27,    29,    21,     0,    31,    18,    59,     0,    58,    53,
       1,    15,     0,     0,     5,     8,    11,     9,     0,    50,
      34,    20,    25,    26,    35,    38,    24,    57,    41,    39,
      60,    48,    45,    43,    28,    30,    22,    47,    32,    12,
       0,     0,     0,    10,     0,     6,     7,    64,    42,    46,
      61,    63,    62,    17
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,     5,    44,    45,     6,    46,    47,    36,    37,
       8,    39
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -41
static const yytype_int8 yypact[] =
{
      21,   -20,   -41,    65,    15,   -41,   -12,    27,   -41,    21,
      -5,   -41,   -41,    -5,    -5,     6,    23,   -41,   -41,   -41,
      -5,    -5,    -5,    -5,    14,   -41,    65,   -41,    -5,   -22,
      -5,    -5,    -5,    -5,    -5,   -41,   -41,     4,   -41,   -41,
     -41,   -41,    21,    27,   -41,    27,   -41,    27,    13,   -41,
     -41,   -41,   -41,   -41,   -41,   -41,   -41,   -41,    26,   -41,
     -41,   -41,    16,   -41,   -41,   -41,   -41,   -41,   -41,   -41,
      65,    65,    65,   -41,    43,   -41,   -41,   -41,   -41,   -41,
     -41,    10,    24,   -41
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -41,   -41,    47,   -40,   -41,   -41,     2,   -41,   -41,   -25,
      -3,    -4
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      38,    60,     7,    74,    25,    75,    49,    76,     9,    50,
      51,     7,    69,    62,    41,    40,    54,    55,    56,    57,
      59,    25,    77,    38,    61,    63,    64,    65,    66,    67,
      68,    42,    52,    70,    71,     1,    72,     2,    58,    70,
      25,     1,    72,     2,    73,    80,    81,    82,     3,    53,
      78,    79,    83,    70,     3,    43,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    38,    38,    38,
      10,    11,    12,     0,     0,    13,    14,    15,    16,     1,
       0,    17,    18,    19,     0,    20,    21,    22,    23,    24,
       0,    25,     0,     0,     0,     0,    26,     0,    27,    28,
      29,     0,    30,    31,    32,     0,     0,    33,    34,    35
};

static const yytype_int8 yycheck[] =
{
       3,    26,     0,    43,    26,    45,    10,    47,    28,    13,
      14,     9,     8,    35,    26,     0,    20,    21,    22,    23,
      24,    26,     9,    26,    28,    29,    30,    31,    32,    33,
      34,     4,    26,    29,    30,    14,    32,    16,    24,    29,
      26,    14,    32,    16,    42,    70,    71,    72,    27,    26,
      24,    35,     9,    29,    27,    28,     9,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
       5,     6,     7,    -1,    -1,    10,    11,    12,    13,    14,
      -1,    16,    17,    18,    -1,    20,    21,    22,    23,    24,
      -1,    26,    -1,    -1,    -1,    -1,    31,    -1,    33,    34,
      35,    -1,    37,    38,    39,    -1,    -1,    42,    43,    44
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    14,    16,    27,    46,    47,    50,    51,    55,    28,
       5,     6,     7,    10,    11,    12,    13,    16,    17,    18,
      20,    21,    22,    23,    24,    26,    31,    33,    34,    35,
      37,    38,    39,    42,    43,    44,    53,    54,    55,    56,
       0,    26,     4,    28,    48,    49,    51,    52,    47,    56,
      56,    56,    26,    26,    56,    56,    56,    56,    24,    56,
      54,    56,    35,    56,    56,    56,    56,    56,    56,     8,
      29,    30,    32,    51,    48,    48,    48,     9,    24,    35,
      54,    54,    54,     9
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


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
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

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
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {
      case 47: /* "chemInfo" */
#line 198 "msmartsParse.yy"
	{ if ((yyvaluep->smartsRoot)!=NULL) delete ((yyvaluep->smartsRoot));};
#line 1248 "msmartsParse.tab.cc"
	break;
      case 48: /* "chain" */
#line 200 "msmartsParse.yy"
	{ if ((yyvaluep->bondListMatchNode)!=NULL) delete ((yyvaluep->bondListMatchNode)); };
#line 1253 "msmartsParse.tab.cc"
	break;
      case 49: /* "bondAtomTest" */
#line 204 "msmartsParse.yy"
	{ if ((yyvaluep->bondTest)!=NULL) delete ((yyvaluep->bondTest)); };
#line 1258 "msmartsParse.tab.cc"
	break;
      case 50: /* "acyclicAtomTest" */
#line 205 "msmartsParse.yy"
	{ if ((yyvaluep->atomOrBondMatchNode)!=NULL) delete ((yyvaluep->atomOrBondMatchNode)); };
#line 1263 "msmartsParse.tab.cc"
	break;
      case 51: /* "atomTest" */
#line 202 "msmartsParse.yy"
	{ if ((yyvaluep->atomOrBondMatchNode)!=NULL) delete ((yyvaluep->atomOrBondMatchNode)); };
#line 1268 "msmartsParse.tab.cc"
	break;
      case 52: /* "branch" */
#line 201 "msmartsParse.yy"
	{ if ((yyvaluep->bondListMatchNode)!=NULL) delete ((yyvaluep->bondListMatchNode)); };
#line 1273 "msmartsParse.tab.cc"
	break;
      case 53: /* "atomPrimativeTest" */
#line 203 "msmartsParse.yy"
	{ if ((yyvaluep->atomOrBondMatchNode)!=NULL) delete ((yyvaluep->atomOrBondMatchNode)); };
#line 1278 "msmartsParse.tab.cc"
	break;
      case 54: /* "logOp" */
#line 206 "msmartsParse.yy"
	{ if ((yyvaluep->logical)!=NULL) delete ((yyvaluep->logical)); };
#line 1283 "msmartsParse.tab.cc"
	break;
      case 55: /* "recursiveChemInfo" */
#line 199 "msmartsParse.yy"
	{ if ((yyvaluep->root)!=NULL) delete ((yyvaluep->root)); };
#line 1288 "msmartsParse.tab.cc"
	break;

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

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

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

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

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 211 "msmartsParse.yy"
    { _BLOCK_TRACE("input: chemInfo");
	    chemInfoTree = (yyvsp[(1) - (1)].smartsRoot)->_obj; 
	;}
    break;

  case 3:
#line 215 "msmartsParse.yy"
    { (yyval.smartsRoot) = new Hold<chemInfo::O_SmartsRoot>(msmartsEnv); ;}
    break;

  case 4:
#line 217 "msmartsParse.yy"
    { _BLOCK_TRACE("chemInfo: atomTest");
		    (yyval.smartsRoot) = new Hold<chemInfo::O_SmartsRoot>(chemInfo::O_SmartsRoot::create((yyvsp[(1) - (1)].atomOrBondMatchNode)->_obj,msmartsEnv)); 
		;}
    break;

  case 5:
#line 221 "msmartsParse.yy"
    { _BLOCK_TRACE("chemInfo:atomTest chain");
		    (yyval.smartsRoot) = new Hold<chemInfo::O_SmartsRoot>(chemInfo::O_SmartsRoot::create((yyvsp[(1) - (2)].atomOrBondMatchNode)->_obj,(yyvsp[(2) - (2)].bondListMatchNode)->_obj,msmartsEnv)); 
		;}
    break;

  case 6:
#line 228 "msmartsParse.yy"
    { _BLOCK_TRACE("chain:bondAtomTest chain");
	(yyval.bondListMatchNode) = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Chain::create( (yyvsp[(1) - (2)].bondTest)->_obj, (yyvsp[(2) - (2)].bondListMatchNode)->_obj,msmartsEnv)); 
    ;}
    break;

  case 7:
#line 232 "msmartsParse.yy"
    { _BLOCK_TRACE("chain: branch chain");
	(yyval.bondListMatchNode) = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Branch::create( (yyvsp[(1) - (2)].bondListMatchNode)->_obj, (yyvsp[(2) - (2)].bondListMatchNode)->_obj,msmartsEnv)); 
    ;}
    break;

  case 8:
#line 236 "msmartsParse.yy"
    { _BLOCK_TRACE("chain: bondAtomTest");
	(yyval.bondListMatchNode) = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Chain::create( (yyvsp[(1) - (1)].bondTest)->_obj,msmartsEnv)); 
    ;}
    break;

  case 9:
#line 240 "msmartsParse.yy"
    { _BLOCK_TRACE("chain: branch");
	(yyval.bondListMatchNode) = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Branch::create( (yyvsp[(1) - (1)].bondListMatchNode)->_obj,msmartsEnv)); 
    ;}
    break;

  case 10:
#line 246 "msmartsParse.yy"
    { _BLOCK_TRACE("bondAtomTest:APBond atomTest");
			    (yyval.bondTest) = new Hold<chemInfo::O_BondTest>(chemInfo::O_BondTest::create( (yyvsp[(1) - (2)].benum), (yyvsp[(2) - (2)].atomOrBondMatchNode)->_obj,msmartsEnv )); 
			;}
    break;

  case 11:
#line 250 "msmartsParse.yy"
    { _BLOCK_TRACE("bondAtomTest: atomTest");
			    (yyval.bondTest) = new Hold<chemInfo::O_BondTest>(chemInfo::O_BondTest::create( chemInfo::SABSingleOrAromaticBond, (yyvsp[(1) - (1)].atomOrBondMatchNode)->_obj,msmartsEnv)); 
			;}
    break;

  case 12:
#line 271 "msmartsParse.yy"
    { _BLOCK_TRACE("acyclicAtomTest: APOpenBracket logOp APCloseBracket");
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>((yyvsp[(2) - (3)].logical)->_obj);
	;}
    break;

  case 13:
#line 275 "msmartsParse.yy"
    { _BLOCK_TRACE("acyclicAtomTest: APOrganicElement");
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>((chemInfo::O_AtomTest::create(chemInfo::SAPElement, 0, 0, (yyvsp[(1) - (1)].eval) ,msmartsEnv))); 
	;}
    break;

  case 14:
#line 279 "msmartsParse.yy"
    { _BLOCK_TRACE("acyclicAtomTest: recursiveChemInfo");
	    		    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>((yyvsp[(1) - (1)].root)->_obj); 
			;}
    break;

  case 15:
#line 288 "msmartsParse.yy"
  { _BLOCK_TRACEF(BF("atomTest: acyclicAtomTest APNumber   // atomTag==(%s)  ") (yyvsp[(2) - (2)].cval));
	(yyval.atomOrBondMatchNode)=new Hold<chemInfo::O_AtomOrBondMatchNode>(chemInfo::O_TagSet::create(chemInfo::SABAnyBond,(yyvsp[(1) - (2)].atomOrBondMatchNode)->_obj,(yyvsp[(2) - (2)].cval),msmartsEnv));
    ;}
    break;

  case 16:
#line 292 "msmartsParse.yy"
    { _BLOCK_TRACE("atomTest: acyclicAtomTest");
	(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>((yyvsp[(1) - (1)].atomOrBondMatchNode)->_obj); 
    ;}
    break;

  case 17:
#line 296 "msmartsParse.yy"
    { (yyval.bondListMatchNode) = new Hold<chemInfo::O_BondListMatchNode>((yyvsp[(2) - (3)].bondListMatchNode)->_obj); ;}
    break;

  case 18:
#line 317 "msmartsParse.yy"
    { 
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>(
				chemInfo::O_AtomTest::create(chemInfo::SAPWildCard,msmartsEnv)); 
	;}
    break;

  case 19:
#line 328 "msmartsParse.yy"
    { 
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( 
				chemInfo::O_AtomTest::create( chemInfo::SAPDegree, 1,msmartsEnv)); 
	;}
    break;

  case 20:
#line 333 "msmartsParse.yy"
    { 
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( 
			chemInfo::O_AtomTest::create( chemInfo::SAPDegree,(yyvsp[(2) - (2)].ival),msmartsEnv)); 
	;}
    break;

  case 21:
#line 345 "msmartsParse.yy"
    { 
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>(
				chemInfo::O_AtomTest::create( chemInfo::SAPTotalHCount, 1,msmartsEnv));
	;}
    break;

  case 22:
#line 350 "msmartsParse.yy"
    { 
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>(
				chemInfo::O_AtomTest::create( chemInfo::SAPTotalHCount, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 23:
#line 361 "msmartsParse.yy"
    {
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( 
			chemInfo::O_AtomTest::create( chemInfo::SAPImplicitHCount, 1,msmartsEnv)); 
	;}
    break;

  case 24:
#line 366 "msmartsParse.yy"
    { 
		(yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>(
					chemInfo::O_AtomTest::create( 
						chemInfo::SAPImplicitHCount, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 25:
#line 378 "msmartsParse.yy"
    {
		(yyval.atomOrBondMatchNode)=new Hold<chemInfo::O_AtomOrBondMatchNode>(
					chemInfo::O_AtomTest::create(chemInfo::SAPRingTest,(yyvsp[(2) - (2)].cval),msmartsEnv));
	;}
    break;

  case 26:
#line 389 "msmartsParse.yy"
    {
		(yyval.atomOrBondMatchNode)=new Hold<chemInfo::O_AtomOrBondMatchNode>(
					chemInfo::O_AtomTest::create(chemInfo::SAPResidueTest,(yyvsp[(2) - (2)].cval),msmartsEnv));
	;}
    break;

  case 27:
#line 401 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingMembershipCount, 1,msmartsEnv)); 
	;}
    break;

  case 28:
#line 405 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingMembershipCount, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 29:
#line 416 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingSize, 1,msmartsEnv)); 
	;}
    break;

  case 30:
#line 420 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingSize, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 31:
#line 429 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPValence, 1,msmartsEnv)); 
	;}
    break;

  case 32:
#line 433 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPValence, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 33:
#line 443 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPConnectivity, 1,msmartsEnv)); 
	;}
    break;

  case 34:
#line 447 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPConnectivity, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 35:
#line 453 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPGroupNumber, (yyvsp[(2) - (2)].ival),msmartsEnv)); 
	;}
    break;

  case 36:
#line 457 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPElectronegativeElement,msmartsEnv)); 
	;}
    break;

  case 37:
#line 461 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPHeavyAtomTotalBond, 1,msmartsEnv)); 
	;}
    break;

  case 38:
#line 465 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPHeavyAtomTotalBond, (yyvsp[(2) - (2)].ival),msmartsEnv)); 
	;}
    break;

  case 39:
#line 478 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 40:
#line 482 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, -1,msmartsEnv));
	;}
    break;

  case 41:
#line 492 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, -2,msmartsEnv));
	;}
    break;

  case 42:
#line 501 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, -3,msmartsEnv));
	;}
    break;

  case 43:
#line 517 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 44:
#line 521 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, 1,msmartsEnv));
	;}
    break;

  case 45:
#line 530 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, 2,msmartsEnv));
	;}
    break;

  case 46:
#line 539 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, 3,msmartsEnv));
	;}
    break;

  case 47:
#line 543 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPTransitionMetal, (yyvsp[(2) - (2)].ival),msmartsEnv)); 
	;}
    break;

  case 48:
#line 547 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPAromaticPiElectron , (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 49:
#line 551 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPiBondOrbital,msmartsEnv)); 
	;}
    break;

  case 50:
#line 560 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPAtomicNumber, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 51:
#line 564 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPChiralityAntiClockwise,msmartsEnv));
	;}
    break;

  case 52:
#line 568 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPChiralityClockwise,msmartsEnv));
	;}
    break;

  case 53:
#line 577 "msmartsParse.yy"
    {
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPAtomicMass, (yyvsp[(1) - (1)].ival),msmartsEnv));
	;}
    break;

  case 54:
#line 581 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPElement, (yyvsp[(1) - (1)].eval) ,msmartsEnv)); 
	;}
    break;

  case 55:
#line 585 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPElement, (yyvsp[(1) - (1)].eval) ,msmartsEnv)); 
	;}
    break;

  case 56:
#line 589 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPLonePair, 1,msmartsEnv)); 
	;}
    break;

  case 57:
#line 593 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPLonePair, (yyvsp[(2) - (2)].ival),msmartsEnv));
	;}
    break;

  case 58:
#line 602 "msmartsParse.yy"
    { 
	    (yyval.atomOrBondMatchNode) = new Hold<chemInfo::O_AtomOrBondMatchNode>((yyvsp[(1) - (1)].root)->_obj); 
	;}
    break;

  case 59:
#line 679 "msmartsParse.yy"
    { 
	    (yyval.logical) = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logIdentity, (yyvsp[(1) - (1)].atomOrBondMatchNode)->_obj ,msmartsEnv)); 
	;}
    break;

  case 60:
#line 689 "msmartsParse.yy"
    { 
	    (yyval.logical) = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logNot, (yyvsp[(2) - (2)].logical)->_obj ,msmartsEnv)); 
	;}
    break;

  case 61:
#line 699 "msmartsParse.yy"
    { 
	    (yyval.logical) = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logHighPrecedenceAnd, (yyvsp[(1) - (3)].logical)->_obj, (yyvsp[(3) - (3)].logical)->_obj ,msmartsEnv)); 
	;}
    break;

  case 62:
#line 709 "msmartsParse.yy"
    { 
	    (yyval.logical) = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logOr, (yyvsp[(1) - (3)].logical)->_obj, (yyvsp[(3) - (3)].logical)->_obj ,msmartsEnv)); 
	;}
    break;

  case 63:
#line 720 "msmartsParse.yy"
    { 
	    (yyval.logical) = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logLowPrecedenceAnd, (yyvsp[(1) - (3)].logical)->_obj, (yyvsp[(3) - (3)].logical)->_obj ,msmartsEnv)); 
	;}
    break;

  case 64:
#line 726 "msmartsParse.yy"
    { (yyval.root) = new Hold<chemInfo::O_Root>((yyvsp[(3) - (4)].smartsRoot)->_obj); ;}
    break;

  case 65:
#line 729 "msmartsParse.yy"
    { (yyval.ival) = atoi((yyvsp[(1) - (1)].cval)); ;}
    break;


/* Line 1267 of yacc.c.  */
#line 2053 "msmartsParse.tab.cc"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

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

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 734 "msmartsParse.yy"


    //
    //
    //	Epilogue
    //
    //


    typedef	struct {
    char	rchar;  
    char* name ; 
    int	token ; 
    }	SpecialCharacterEntry;

    #if 0 
    SpecialCharacterEntry specialCharacters[] = {
    { ';', "APOperatorAndLow", APOperatorAndLow },
    { ',', "APOperatorOr", APOperatorOr },
    { '!', "APOperatorNot", APOperatorNot },
    { '&', "APOPeratorAndHigh", APOperatorAndHigh },
    { '+', "APPositiveCharge", APPositiveCharge },
    //{ '*', "APWildcardOrAtomicMass", APWildcardOrAtomicMass },   
    { '*', "APWildCard", APWildCard},   
    { '=', "APDoubleBond", APDoubleBond },
    { ':', "APAromaticBond", APAromaticBond },
    { '~', "APAnyBond", APAnyBond },
    { '$', "APDollarSign", APDollarSign},
    { '(', "APOpenParenthesis", APOpenParenthesis },
    { ')', "APCloseParenthesis", APCloseParenthesis },
    { '[', "APOpenBracket", APOpenBracket },
{ ']', "APCloseBracket", APCloseBracket },
{ 'D', "APDegree", APDegree },
{ 'H', "APTotalHCount", APTotalHCount },
{ 'h', "APImplicitHCount", APImplicitHCount },
{ 'i', "APPiBondOrbital", APPiBondOrbital },
{ 'Q', "APHeavyAtomTotalBond", APHeavyAtomTotalBond },
{ 'R', "APRingMembershipCount", APRingMembershipCount }, 
{ 'r', "APRingSize", APRingSize },
{ 'v', "APValence", APValence },
{ 'X', "APConnectivity", APConnectivity },
{ '\0', "", 0 }
};
#endif


const char*	elementTable[] = 
{		//First the two character Inorganic elements
"Al",
"Ca",		
"Co",
"Cu",
"Fe",
""		// Last entry
};


const char*	organicElementTable[] = 
{
"Br",       	//Put 2 chars elements on top, or element won't match correctly 
"Cl",
"B",
"b",
"C",
"c",
"F",
"I",
"N",
"n",
"O",
"o",
"P",
"p",
"S",
"s",
""
};

int	yylex() 
{
char	ichar;
    do 
    {
	ichar = lexStream->get();	 // Read one character
	if (ichar=='\n') 
	return APEndOfLine;
    } while (!lexStream->eof() && ichar < ' ' );
    if ( lexStream->eof() ) return 0;
#ifdef	DEBUG_ON
    switch (ichar)
    {
        case '&':
	    LEXDPRINT(("next char: &amp;\n"));
	    break;
	case '<':
	    LEXDPRINT(("next char: &lt;\n"));
	    break;
	case '>':
	    LEXDPRINT(("next char: &gt;\n"));
	    break;
	default:
            LEXDPRINT(("next char: |%c|\n", ichar));
	    break;
    }
#endif


		    //
		    // Handle two and one character elements
		    //
    LEXDPRINT(("Checking 1 and 2 chars inorganic elements\n"));	
    for ( int i=0; elementTable[i][0]!='\0'; i++ )
    {
	if ( elementTable[i][0] == ichar ) 
	{
	    if (elementTable[i][1] != '\0') 
	    {
		if ( elementTable[i][1] == lexStream->peek() ) 
		    {
		    lexStream->get();
//			strcpy( yylval.eval ,  elementTable[i] );
		    yylval.eval = elementTable[i];
		    LEXDPRINT(("Matched the 2 char inorganic element: |%s|\n",elementTable[i]));	
		    return APInorganicElement;
		    }
	    } 
	    else 
	    {

		yylval.eval = elementTable[i];
//		    strcpy( yylval.eval , elementTable[i]);
		LEXDPRINT(("Matched the 1 char inorganic element: |%s|\n",elementTable[i]));	
		return APInorganicElement;
	    }
       }
   }            



// 	Process Both Aliphatic and Aromatic Organic ElementS	
    LEXDPRINT(("Checking 1 and 2 chars organic elements\n"));	
    for (int i=0; organicElementTable[i][0] != '\0' ; i++)
    {	
	if (organicElementTable[i][0] == ichar )   
	{
	    LEXDPRINT(("Matched the 1st char in organicElementTable[]\n"));
	    if (organicElementTable[i][1] != '\0')
	    {
		if ( organicElementTable[i][1] == lexStream->peek() )
		{
		    cout<<"Peek char: "<<lexStream->peek()<<std::endl;
		    lexStream->get();
//			 strcpy(yylval.eval , organicElementTable[i] );
		    yylval.eval = organicElementTable[i];
		    LEXDPRINT(("Matched the 2 char organic element: |%s|\n", elementTable[i]));
		    return APOrganicElement;
		}
	    }	
	    else 
	    {
		yylval.eval = organicElementTable[i];
//		     strcpy( yylval.eval, organicElementTable[i];
		LEXDPRINT(("Matched the 1 char organic element: |%s|\n",organicElementTable[i]));
		return APOrganicElement;
	    }	
	}
    } 





		    //	
		    // Handle special characters that are one
		    // character long
		    //

    LEXDPRINT(("Checking special characters\n"));	
    switch (ichar) 
    {
	    case '*':
		    LEXPRINT("APWildCard");
		    return APWildCard;
		    break;
	    case '$':
		    LEXPRINT("APDollarSign");
		    return APDollarSign;
		    break;
	    case '(':
		    LEXPRINT("APOpenParenthesis");
		    return APOpenParenthesis;
		    break;
	    case ')':
		    LEXPRINT("APCloseParenthesis");
		    return APCloseParenthesis;
		    break;
	    case '[':
		    LEXPRINT("APOpenBracket");
		    return APOpenBracket;
		    break;
	    case ']':
		    LEXPRINT("APCloseBracket");
		    return APCloseBracket;
		    break;
	    case ';':
		    LEXPRINT("APOperatorAndLow");
		    return APOperatorAndLow;
		    break;
	    case '?' :
	    	    LEXPRINT("APRingTest");
		    return APRingTest;
		    break;
	    case 'U':	
	    		// Test if residue of this atom
			// is the same as a previously tagged atom
		    LEXPRINT("APResidueTest");
		    LEXDPRINT(("Identified as APResidueTest\n"));
		    return APResidueTest;
		    break;
	    case ',':
		    LEXPRINT("APOperatorOr");
		    return APOperatorOr;
		    break;
	    case '!':
		    LEXPRINT("APOperatorNot");
		    return APOperatorNot;
		    break;
	    case '&':
		    LEXPRINT("APOpeartorAndHigh");
		    return APOperatorAndHigh;
		    break;
	    case '~':
		    LEXPRINT("APAnyBond");   
                    yylval.benum = chemInfo::SABAnyBond;
		    return APBond;
		    break;	

	    case ':' :
		    LEXPRINT("APAromaticBond");
                    yylval.benum = chemInfo::SABAromaticBond;
		    return APBond;
		    break;


	    case '#':
		    if (lexStream->peek() >= '0' && lexStream->peek() <= '9' ) 
		    {
			    LEXPRINT("APAtomicNumber");
			    return APAtomicNumber;
			    break;
		    } else if (lexStream->peek() == 'G')
		    {
			    lexStream->get();
			    if (lexStream->peek() >= '0' && lexStream->peek() <= '9')
			    {
			        LEXPRINT("APGroupNumber");
				return APGroupNumber;
				break;
			    }
			    return APError;
		    } else if (lexStream->peek() == 'T')
		    {
			    lexStream->get();
			    if (lexStream->peek() >= '0' && lexStream->peek() <= '9')
				    {
				    LEXPRINT("APTransitionMetal");
				    return APTransitionMetal;
				    break;
				    }
			    return APError;
		    } else if (lexStream->peek() == 'N')
		    {
			    lexStream->get();
			    if (lexStream->peek() >= '0' && lexStream->peek() <= '9')
				    {
				    LEXPRINT("APElectronegativeElement");
				    return APElectronegativeElement;
				    break;
				    }
			    return APError;
		    } 
		    LEXPRINT("APTripleBond");
                    yylval.benum = chemInfo::SABTripleBond;
		    return APBond;
		    break;
	    case '/':
		    if (lexStream->peek() == '?') 
		    {
			lexStream->get();
			LEXPRINT("APSingleUpOrUnspecified");
			yylval.benum = chemInfo::SABDirectionalSingleUpOrUnspecified;
			return  APBond;
			break;
		    }
		    LEXPRINT("APDirectionalSingleUp");
		    yylval.benum = chemInfo::SABDirectionalSingleUp;
		    return  APBond;
		    break;
	    case '-':
		    if (lexStream->peek() >= '0' && lexStream->peek() <= '9'  ) 
		    {	
			    LEXPRINT("APNegativeCharge");
			    return APNegativeCharge;
			    break; 
		    }
		    LEXPRINT("APSingleBond");
		    yylval.benum = chemInfo::SABSingleBond;
		    return  APBond;
		    break;
	    case '=':
		    LEXPRINT("APDoubleBond");
                    yylval.benum = chemInfo::SABDoubleBond;
		    return APBond;
		    break;	
	    case '\\':
		    if (lexStream->peek() == '?') 
		    {
			lexStream->get();
			LEXPRINT("APSingleDownOrUnspecified");
			yylval.benum = chemInfo::SABDirectionalSingleDownOrUnspecified;
			return  APBond;
			break; 
		    }
		    LEXPRINT("APDirectionalSingleDown");
		    yylval.benum = chemInfo::SABDirectionalSingleDown;
		    return  APBond;
		    break;
	    case '@':
		    if ( lexStream->peek() == '@' ) {
			lexStream->get(); // pull the second @ out of the stream
			LEXPRINT("APChiralityClockwise");
			LEXDPRINT(("Identified as APChiralityClockwise\n"));	
			return  APChiralityClockwise;
			break;
		    }
		    LEXPRINT("APChiralityAntiClockwiseOrAnyRingBond");
		    LEXDPRINT(("Identified as APChiralityAntiClockwise\n"));
		    return  APChiralityAntiClockwise;
                    break;
	    case 'D':
		    if ( lexStream->peek() >= '0' && lexStream->peek() <= '9') {
		        LEXPRINT("APDegree");
		        LEXDPRINT(("Identified as APDegree\n"));
		        return APDegree;
		        break;
		    }
		    return APError;
		    break;
	    case 'H':
		    LEXPRINT("APTotalHCount");
		    LEXDPRINT(("Identified as APTotalHCount\n"));
		    return  APTotalHCount;
		    break; 
	    case 'h':
		    LEXPRINT("APImplicitHCount");
		    LEXDPRINT(("Identifed as APImplicitHCount\n"));
		    return  APImplicitHCount;
		    break;
	    case 'i':
		    if (lexStream->peek() >= '0' && lexStream->peek() <= '9')
		    {
			LEXPRINT("APAromaticPiElectron");
			LEXDPRINT(("Identified as APAromaticPiElectron\n"));
			return APAromaticPiElectron;
			break;
		    }
		    LEXPRINT("APPiBondOrbital");
		    LEXDPRINT(("Identified as APPiBondOrbital\n"));
		    return APPiBondOrbital;
		    break;
	    case 'Q':
		    LEXPRINT("APHeavyAtomTotalBond");
		    LEXDPRINT(("Identified as APHeavyAtomTotalBond\n"));
		    return APHeavyAtomTotalBond;
		    break;  
	    case 'R':
		    LEXPRINT("APRingMembershipCount");
		    LEXDPRINT(("Identified as APRingMembership\n"));
		    return APRingMembershipCount;
		    break;
	    case 'r':
		    LEXPRINT("APRingSize");
		    LEXDPRINT(("Identified as APRingSize\n"));
		    return APRingSize;
		    break;
	    case 'v':
		    LEXPRINT("APValence");
		    LEXDPRINT(("Identified as APValence\n"));	
		    return APValence;
		    break;
	    case 'X':
		    LEXPRINT("APConnectivity");  
		    LEXDPRINT(("Identified as APConnectivity\n"));	
		    return APConnectivity;
		    break;
    }
		    //	
		    // Handle special characters that are two 
		    // characters long
		    //
    if (ichar == 'L' && lexStream->peek() == 'p')
    {
        lexStream->get();	
        LEXPRINT("APLonePair");
        return APLonePair;
    }

			    // Parse a number
    if ( ichar>='0' && ichar<='9' ) 
    {
	    //Create a string for number	
	    int digitCount = 0;
	    yylval.cval[digitCount] = ichar;
	    digitCount++;
	    while ( lexStream->peek()>='0' && lexStream->peek()<='9' ) 
	    {
		    ichar = lexStream->get();
		    if ( digitCount >= MAX_CVAL )
		    {
		        TOSS(_lisp->create<O_LispError>("Exceeded max number of digits in Number"));
		    }
		    yylval.cval[digitCount] = ichar;
		    digitCount++;
	    }
	    yylval.cval[digitCount] = '\0';
	    LOG(BF("APNumber = (%s)") % yylval.cval ); // vp0(("APNumber = (%s)",yylval.cval));
	    return  APNumber;
    }
    

    LEXPRINT("APError");
    return APError;


}


void	yyerror(const char* str) {
    chemInfoErrorStream << "SMARTS error: " << str << std::endl;
}


#if 0
int main()
{
    cout<<"Input CHEM_INFO : ";
    lexStream = &cin;

    yyparse();


return 0;
}
#endif


chemInfo::RPSmartsRoot smartsCompile(const string& input, RPLisp env)
{_F(this->lisp());
stringstream	sin;
int		status;
    msmartsEnv = env;
    sin.str(input);
    lexStream = &sin;
    chemInfoTree = chemInfo::_Nil<O_SmartsRoot>();
    atomTagCounter.clear();
    chemInfoErrorStream.str("ChemInfo: ");
    status = yyparse();
    return chemInfoTree;
}


string	chemInfoError()
{
    return chemInfoErrorStream.str();
}

    

void 	testChemInfoString()
{
	cout<<"Input CHEM_INFO : ";
	lexStream = &cin;
	yyparse();
}
