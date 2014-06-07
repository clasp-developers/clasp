/*
 * Defines for a generic argv and argc processor...
 *
 * Copyright 1995 by Gray Watson
 *
 * This file is part of the argv library.
 *
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Gray Watson not be used in advertising
 * or publicity pertaining to distribution of the document or software
 * without specific, written prior permission.
 *
 * Gray Watson makes no representations about the suitability of the
 * software described herein for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The author may be contacted via http://dmalloc.com/
 *
 * $Id: dmalloc_argv.h,v 1.12 2005/12/18 14:47:02 gray Exp $
 */

#ifndef __ARGV_H__
#define __ARGV_H__

/*
 * Version string for the library
 *
 * NOTE to gray: whenever this is changed, corresponding Changlog and
 * NEWS entries *must* be entered and 2 entries in argv.texi must be
 * updated.
 *
 * ARGV LIBRARY VERSION -- 2.5.0
 */

/* produced by configure, inserted into argv.h */
/* used to handle the const operator */
/* const is available */

/* NOTE: start of $Id: dmalloc_argv.h,v 1.12 2005/12/18 14:47:02 gray Exp $ */

/*
 * Generic and standardized argument processor.  You describe the arguments
 * that you are looking for along with their types and these routines do the
 * work to convert them into values.
 *
 * These routines also provide standardized error and usage messages as well
 * as good usage documentation and long and short options.
 */

#include <stdio.h>			/* have to for FILE * below */

/* this defines what type the standard void memory-pointer is */
#if (defined(__STDC__) && __STDC__ == 1) || defined(__cplusplus)
#define ARGV_PNT	void *
#else
#define ARGV_PNT	char *
#endif

/*
 * argument information structure.  this specifies the allowable options
 * and some information about each one.
 *
 * { 'O',  "optimize",  ARGV_BOOL,  &optimize,  NULL,  "turn on optimization" }
 * { 'c',  "config",  ARGV_CHAR_P,  &config,  "file",  "configuration file" }
 */
typedef struct {
  char		ar_short_arg;		/* short argument, 'd' if '-d' */
  char		*ar_long_arg;		/* long version of arg, '--delete' */
  unsigned int	ar_type;		/* type of option, see values below */
  ARGV_PNT	ar_variable;		/* address of associated variable */
  char		*ar_var_label;		/* label for variable description */
  char		*ar_comment;		/* comment for usage message */
} argv_t;

/*
 * Argument array type.  when ARGV_FLAG_ARRAY is |'d with the ar_type
 * in the above structure then multiple instances of the option are
 * allowed and each instance is stored into the following structure
 * that MUST be in ar_variable in the above arg_t structure.
 *
 * NOTE: after the arguments have been processed, if aa_entryn is > 0
 * then aa_entries needs to be free'd by user. argv_cleanup() can be
 * used for this
 */
typedef struct {
  int		aa_entry_n;		/* number of elements in aa_entrees */
  ARGV_PNT	aa_entries;		/* entry list specified */
} argv_array_t;

/*  extract the count of the elements from an argv ARRAY */
#define ARGV_ARRAY_COUNT(array)		((array).aa_entry_n)

/* extract WHICH entry of TYPE from an argv ARRAY */
#define ARGV_ARRAY_ENTRY(array, type, which)	\
	(((type *)(array).aa_entries)[which])

/* extract a pointer to WHICH entry of TYPE from an argv ARRAY */
#define ARGV_ARRAY_ENTRY_P(array, type, which)	\
	(((type *)(array).aa_entries) + which)

/* special ar_short_arg value to mark the last entry in the argument array */
#define ARGV_LAST	((char)255)

/*
 * special ar_short_arg value to mark mandatory arguments (i.e. arguments that
 * *must* be specified.  for arguments that are not optional like [-b].
 * to have a variable number of mandatory args then make the last MAND
 * entry be a ARG_ARRAY type.
 */
#define ARGV_MAND	((char)254)

/*
 * special ar_short_arg value to mark that there is the possibility of
 * a mandatory argument here if one is specified.
 */
#define ARGV_MAYBE	((char)253)

/*
 * special ar_short_arg value to say that the previous and next arguments in
 * the list should not be used together.
 * {'a'...}, {ARG_OR}, {'b'...}, {ARG_OR}, {'c'...} means
 * the user should only specific -a or -b or -c but not 2 or more.
 */
#define ARGV_OR		((char)252)

/*
 * special ar_short_arg value that is the same as ARGV_OR but one of the args
 * must be used.
 * {'a'...}, {ARG_ONE_OF}, {'b'...}, {ARG_ONE_OF}, {'c'...} means
 * the user must specify one of -a or -b or -c but not 2 or more.
 * ARGV_XOR is there for compatibility with older versions.
 */
#define ARGV_ONE_OF	((char)251)
#define ARGV_XOR	((char)251)

/*
 * ar_type values of arg_t
 * NOTE: if this list is changed, some defines in argv_loc need to be changed
 */
#define ARGV_BOOL	1		/* boolean type, sets to ARGV_TRUE */
#define ARGV_BOOL_NEG	2		/* like bool but sets to ARGV_FALSE */
#define ARGV_BOOL_ARG	3		/* like bool but takes a yes/no arg */
#define ARGV_CHAR	4		/* single character */
#define ARGV_CHAR_P	5		/* same as STRING */
#define ARGV_SHORT	6		/* short integer number */
#define ARGV_U_SHORT	7		/* unsigned short integer number */
#define ARGV_INT	8		/* integer number */
#define ARGV_U_INT	9		/* unsigned integer number */
#define ARGV_LONG	10		/* long integer number */
#define ARGV_U_LONG	11		/* unsinged long integer number */
#define ARGV_FLOAT	12		/* floating pointer number */
#define ARGV_DOUBLE	13		/* double floating pointer number */
#define ARGV_BIN	14		/* binary number (0s and 1s) */
#define ARGV_OCT	15		/* octal number, (base 8) */
#define ARGV_HEX	16		/* hexadecimal number, (base 16) */
#define ARGV_INCR	17		/* int arg which gets ++ each time */
#define ARGV_SIZE	18		/* long arg which knows mMbBkKgG */
#define ARGV_U_SIZE	19		/* u_long arg which knows mMbBkKgG */
#define ARGV_BOOL_INT	20		/* like bool but takes an integer var*/
#define ARGV_BOOL_INT_NEG 21		/* like bool-neg but with an integer */
#define ARGV_BOOL_INT_ARG 22		/* like bool-arg but with an integer */

#define ARGV_TYPE(t)	((t) & 0x3F)	/* strip off all but the var type */
#define ARGV_FLAG_ARRAY	(1 << 14)	/* OR with type to indicate array */
#define ARGV_FLAG_MAND	(1 << 13)	/* OR with type to mark mandatory */
/* NOTE: other internal flags defined in argv_loc.h */

/* argv_usage which argument values */
#define ARGV_USAGE_NONE		0	/* no usage messages -- special */
#define ARGV_USAGE_SHORT	1	/* print short usage messages */
#define ARGV_USAGE_LONG		2	/* print long-format usage messages */
#define ARGV_USAGE_DEFAULT	3	/* default usage messages */
#define ARGV_USAGE_SEE		4	/* say see --usage for more info */
#define ARGV_USAGE_SHORT_REM	5	/* short + reminder how to get long */
#define ARGV_USAGE_ALL		6	/* all usage information */

/* boolean type settings */
#define ARGV_FALSE		0
#define ARGV_TRUE		1

/*<<<<<<<<<<  The below prototypes are auto-generated by fillproto */

/* This is a processed version of argv[0], pre-path removed: /bin/ls -> ls */
extern
char	argv_program[/* PROGRAM_NAME + 1 */];

/* A global value of argv from main after argv_process has been called */
extern
char	**argv_argv;

/* A global value of argc from main after argv_process has been called */
extern
int	argv_argc;

/* This should be set externally to provide general program help to user */
extern
char	*argv_help_string;

/* This should be set externally to provide version information to the user */
extern
char	*argv_version_string;

/*
 * Are we running interactively?  This will exit on errors.  Set to
 * false to return error codes instead.
 */
extern
int 	argv_interactive;

/*
 * The FILE stream that argv out_puts all its errors.  Set to NULL to
 * not dump any error messages.  Default is stderr.
 */
extern
FILE 	*argv_error_stream;

/*
 * This is the error code to exit with when we have a usage error and
 * we are in interactive mode.
 */
extern
int	argv_error_code;

/*
 * Set to 1 (the default) to enable the handling of -l=foo or
 * --logfile=foo type of arguments.  Set to 0 to disable.  This allows
 * you to specifically assign a value to an argument.
 */
extern
int	argv_close_enable_b;

/*
 * If the library sees a "--" argument, it will turn off further
 * argument process.  Set to 1 to enable the ability of specifying
 * additional "--" arguments to reenable (basically toggle on then
 * off) argument processing.  Set to 0 (the default) to disable this
 * behavior.
 */
extern
int	argv_last_toggle_b;

/*
 * Set to 1 (the default) to have the library accept multiple usage of
 * the same argument.  Set to 0 to have the library generate an error
 * if you use an argument twice.
 */
extern
int	argv_multi_accept_b;

/*
 * Set to one of the ARGV_USAGE_ defines in the argv.h file.  This
 * tell the library what usage information to display when --usage is
 * specified by the user.  Default is ARGV_USAGE_LONG.
 */
extern
int	argv_usage_type;

/*
 * Set to one of the ARGV_USAGE_ defines in the argv.h file.  This
 * tell the library what usage information to display when an error is
 * encountered.  The usage information accompanies the error message.
 * Default is ARGV_USAGE_SEE.
 */
extern
int	argv_error_type;

/*
 * Set to 1 (the default) if you want the library look for associated
 * arguments from the associated program's environmental variable.  If
 * set the 0 then no environmental variable will be used.  If you are
 * running program foo then the library will look for the
 * environmental variable ARGV_foo and will add those to the argument
 * list specified on the command line.  By default they will be
 * inserted in front of those on the command line unless the
 * argv_env_after_b is set to 1.
 *
 * NOTE: this is set by argv_process automatically.  If you do not
 * want this behavior, you should use argv_process_no_env.
 */
extern
int	argv_process_env_b;

/*
 * Set to 1 if you want the library to append the arguments from the
 * program's environmental variable after those specified on the
 * command line.  If set the 0 (the default) then they will be
 * inserted before those specified on the command line.  See
 * argv_process_env_b for more information.
 */
extern
int	argv_env_after_b;

/*
 * int argv_process_no_env
 *
 * DESCRIPTION:
 *
 * Process the user arguments with an argv_t structure array.  Like
 * argv_process_args but without the processing of the argv
 * environmental variables.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Failure - -1
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures.
 *
 * arg_n - Number of arguments in the argv array.
 *
 * argv - Array of character pointers terminated by 0L.
 */
extern
int	argv_process_no_env(argv_t *args, const int arg_n, char **argv);

/*
 * int argv_process
 *
 * DESCRIPTION:
 *
 * Processes a number of arguments depending on the argument array.
 * This routine will not modify the argv array in any way.
 *
 * NOTE: it will modify the args array by setting various flags in the
 * type field.  returns 0 if no error else -1.
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures that we are using to process the
 * user argument array.  If null then an empty array is used.
 *
 * argc - Number of arguments in the argv argument array.
 *
 * argv - Array of character pointer arguments terminated by a 0L.
 */
extern
int	argv_process(argv_t *args, const int argc, char **argv);

/*
 * int argv_usage
 *
 * DESCRIPTION:
 *
 * Print the standard usage messages for our argument array.  You can
 * specify whether you want to see a short or long usage messages.
 *
 * NOTE: if this is called before argv_process then the program name
 * may be invalid.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Failure - -1
 *
 * ARGUMENTS:
 *
 * args - Our argument array to print the usage messages about.  If
 * null then an empty array is used.
 *
 * which - Either ARGV_USAGE_SHORT (for short usage messages),
 * ARGV_USAGE_LONG (for long usage messages), or ARGV_USAGE_DEFAULT
 * (the user's default either long or short).
 */
extern
int	argv_usage(const argv_t *args, const int which);

/*
 * int argv_was_used
 *
 * DESCRIPTION:
 *
 * See if an argument was used in a previous call to argv_process.
 *
 * RETURNS:
 *
 * 1 if yes it was used, else 0 if not.
 *
 * ARGUMENTS:
 *
 * args - Argument list to search.
 *
 * short_arg - Short argument to see if it was used.
 */
extern
int	argv_was_used(const argv_t *args, const char short_arg);

/*
 * int argv_long_was_used
 *
 * DESCRIPTION:
 *
 * See if a long argument was used in a previous call to argv_process.
 *
 * RETURNS:
 *
 * 1 if yes it was used, else 0 if not.
 *
 * ARGUMENTS:
 *
 * args - Argument list to search.
 *
 * long_arg - Long argument to see if it was used.
 */
extern
int	argv_long_was_used(const argv_t *args, const char *long_arg);

/*
 * int argv_entry_was_used
 *
 * DESCRIPTION:
 *
 * See if an entry in the argument array was used in a previous call
 * to argv_process.
 *
 * RETURNS:
 *
 * 1 if yes it was used, else 0 if not.
 *
 * ARGUMENTS:
 *
 * argv_entry_p - Pointer to an entry in a argv_t list.
 */
extern
int	argv_entry_was_used(const argv_t *argv_entry_p);

/*
 * void argv_cleanup
 *
 * DESCRIPTION:
 *
 * Frees up any allocations associated with the argument array during
 * argv_process.  This should be done at the end of the program or
 * after all the arguments have been referenced.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * args - Argument array we are cleaning up.
 */
extern
void	argv_cleanup(const argv_t *args);

/*
 * int argv_copy_args
 *
 * DESCRIPTION:
 *
 * Copy all the arguements (not including the 0th) one after the other
 * into the user specified buffer.
 *
 * NOTE: you can get the 0th argument from argv_argv[0] or
 * argv_program.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Failure - -1
 *
 * ARGUMENTS:
 *
 * buf - Buffer to copy all of the user arguments into.
 *
 * buf_size - Size of the buffer.
 */
extern
int	argv_copy_args(char *buf, const int buf_size);

/*
 * int argv_value_string
 *
 * DESCRIPTION:
 *
 * Convert the value of a RC entry to its string equivalent in the
 * buffer provided.
 *
 * RETURNS:
 *
 * Length of bytes copied into the buffer.
 *
 * ARGUMENTS:
 *
 * argv_entry_p - Pointer to an entry in a argv_t list.
 *
 * buf - Buffer to convert the value into.
 *
 * buf_size - Size of the buffer.
 */
extern
int	argv_value_string(const argv_t *argv_entry_p, char *buf,
			  const int buf_size);

/*
 * int argv_type_info
 *
 * DESCRIPTION:
 *
 * Get internal information about the type of the argument.
 *
 * RETURNS:
 *
 * The name of the type.
 *
 * ARGUMENTS:
 *
 * type - Number of argument type.
 *
 * size_p - Pointer to an unsigned integer which, if not NULL, will be
 * set with the size of the type.
 *
 * desc_p - Pointer to a constant character pointer which, if not
 * NULL, will be pointed to a description of the type.
 */
extern
const char	*argv_type_info(const unsigned int type, unsigned int *size_p,
				const char **desc_p);

/*<<<<<<<<<<   This is end of the auto-generated output from fillproto. */

#endif /* ! __ARGV_H__ */
