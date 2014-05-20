/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     _BANG_t = 258,
     _BANG_EQUALS_t = 259,
     _AMPER_t = 260,
     _AMPERAMPER_t = 261,
     _LPAREN_t = 262,
     _RPAREN_t = 263,
     _PLUS_EQUALS_t = 264,
     _COLON_t = 265,
     _SEMIC_t = 266,
     _LANGLE_t = 267,
     _LANGLE_EQUALS_t = 268,
     _EQUALS_t = 269,
     _RANGLE_t = 270,
     _RANGLE_EQUALS_t = 271,
     _QUESTION_EQUALS_t = 272,
     _LBRACKET_t = 273,
     _RBRACKET_t = 274,
     ACTIONS_t = 275,
     BIND_t = 276,
     CASE_t = 277,
     CLASS_t = 278,
     DEFAULT_t = 279,
     ELSE_t = 280,
     EXISTING_t = 281,
     FOR_t = 282,
     IF_t = 283,
     IGNORE_t = 284,
     IN_t = 285,
     INCLUDE_t = 286,
     LOCAL_t = 287,
     MODULE_t = 288,
     ON_t = 289,
     PIECEMEAL_t = 290,
     QUIETLY_t = 291,
     RETURN_t = 292,
     RULE_t = 293,
     SWITCH_t = 294,
     TOGETHER_t = 295,
     UPDATED_t = 296,
     WHILE_t = 297,
     _LBRACE_t = 298,
     _BAR_t = 299,
     _BARBAR_t = 300,
     _RBRACE_t = 301,
     ARG = 302,
     STRING = 303
   };
#endif
#define _BANG_t 258
#define _BANG_EQUALS_t 259
#define _AMPER_t 260
#define _AMPERAMPER_t 261
#define _LPAREN_t 262
#define _RPAREN_t 263
#define _PLUS_EQUALS_t 264
#define _COLON_t 265
#define _SEMIC_t 266
#define _LANGLE_t 267
#define _LANGLE_EQUALS_t 268
#define _EQUALS_t 269
#define _RANGLE_t 270
#define _RANGLE_EQUALS_t 271
#define _QUESTION_EQUALS_t 272
#define _LBRACKET_t 273
#define _RBRACKET_t 274
#define ACTIONS_t 275
#define BIND_t 276
#define CASE_t 277
#define CLASS_t 278
#define DEFAULT_t 279
#define ELSE_t 280
#define EXISTING_t 281
#define FOR_t 282
#define IF_t 283
#define IGNORE_t 284
#define IN_t 285
#define INCLUDE_t 286
#define LOCAL_t 287
#define MODULE_t 288
#define ON_t 289
#define PIECEMEAL_t 290
#define QUIETLY_t 291
#define RETURN_t 292
#define RULE_t 293
#define SWITCH_t 294
#define TOGETHER_t 295
#define UPDATED_t 296
#define WHILE_t 297
#define _LBRACE_t 298
#define _BAR_t 299
#define _BARBAR_t 300
#define _RBRACE_t 301
#define ARG 302
#define STRING 303




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



