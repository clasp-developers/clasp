/*
 * Generic argv processor...
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
 */

#include <ctype.h>
#include <stdio.h>

#if HAVE_STRING_H
# include <string.h>
#endif
#if HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "conf.h"

#include "dmalloc_argv.h"
#include "dmalloc_argv_loc.h"
#include "compat.h"

/* internal routines */
static	void	do_list(argv_t *grid, const int arg_c, char **argv,
			argv_t **queue_list, int *queue_head_p,
			int *queue_tail_p, int *okay_bp);

/*
 * exported variables
 */
/* This is a processed version of argv[0], pre-path removed: /bin/ls -> ls */
char	argv_program[PROGRAM_NAME + 1] = "Unknown";

/* A global value of argv from main after argv_process has been called */
char	**argv_argv = NULL;
/* A global value of argc from main after argv_process has been called */
int	argv_argc = 0;

/* This should be set externally to provide general program help to user */
char	*argv_help_string = NULL;
/* This should be set externally to provide version information to the user */
char	*argv_version_string = NULL;

/*
 * Are we running interactively?  This will exit on errors.  Set to
 * false to return error codes instead.
 */
int 	argv_interactive = ARGV_TRUE;

/*
 * The FILE stream that argv out_puts all its errors.  Set to NULL to
 * not dump any error messages.  Default is stderr.
 */
FILE 	*argv_error_stream = ERROR_STREAM_INIT;

/*
 * This is the error code to exit with when we have a usage error and
 * we are in interactive mode.
 */
int	argv_error_code = EXIT_CODE;

/*
 * global settings
 */

/*
 * Set to 1 (the default) to enable the handling of -l=foo or
 * --logfile=foo type of arguments.  Set to 0 to disable.  This allows
 * you to specifically assign a value to an argument.
 */
int	argv_close_enable_b = 1;

/*
 * If the library sees a "--" argument, it will turn off further
 * argument process.  Set to 1 to enable the ability of specifying
 * additional "--" arguments to reenable (basically toggle on then
 * off) argument processing.  Set to 0 (the default) to disable this
 * behavior.
 */
int	argv_last_toggle_b = 0;

/*
 * Set to 1 (the default) to have the library accept multiple usage of
 * the same argument.  Set to 0 to have the library generate an error
 * if you use an argument twice.
 */
int	argv_multi_accept_b = 1;

/*
 * Set to one of the ARGV_USAGE_ defines in the argv.h file.  This
 * tell the library what usage information to display when --usage is
 * specified by the user.  Default is ARGV_USAGE_LONG.
 */
int	argv_usage_type = ARGV_USAGE_LONG;

/*
 * Set to one of the ARGV_USAGE_ defines in the argv.h file.  This
 * tell the library what usage information to display when an error is
 * encountered.  The usage information accompanies the error message.
 * Default is ARGV_USAGE_SEE.
 */
int	argv_error_type = ARGV_USAGE_SEE;

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
int	argv_process_env_b = 1;

/*
 * Set to 1 if you want the library to append the arguments from the
 * program's environmental variable after those specified on the
 * command line.  If set the 0 (the default) then they will be
 * inserted before those specified on the command line.  See
 * argv_process_env_b for more information.
 */
int	argv_env_after_b = 0;

/*
 * local variables
 */

/* empty argument array */
static	argv_t	empty[] = {{ ARGV_LAST, NULL, 0, NULL, NULL, NULL }};
static	int	enabled_b = ARGV_FALSE;		/* are the lights on? */

/****************************** startup routine ******************************/

/*
 * static void argv_startup
 *
 * DESCRIPTION:
 *
 * Turn on the lights.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
static	void	argv_startup(void)
{
  if (enabled_b) {
    return;
  }
  enabled_b = ARGV_TRUE;
  
  /* ANSI says we cannot predefine this above */
  if (argv_error_stream == ERROR_STREAM_INIT) {
    argv_error_stream = stderr;
  }
}

/***************************** general utilities *****************************/

/*
 * static int btoi
 *
 * DESCRIPTION:
 *
 * Binary string to integer translation.
 *
 * RETURNS:
 *
 * Integer converted from the string.
 *
 * ARGUMENTS:
 *
 * str - String of binary 0s and 1s that we are converting.
 */
static	int	btoi(const char *str)
{
  int		ret = 0;
  
  /* strip off spaces */
  for (; isspace(*str); str++) {
  }
  
  for (; *str == '0' || *str == '1'; str++) {
    ret *= 2;
    ret += *str - '0';
  }
  
  return ret;
}

/*
 * static int otoi
 *
 * DESCRIPTION:
 *
 * Octal string to integer translation.
 *
 * RETURNS:
 *
 * Integer converted from the string.
 *
 * ARGUMENTS:
 *
 * str - String of octal digits that we are converting.
 */
static	int	otoi(const char *str)
{
  int		ret = 0;
  
  /* strip off spaces */
  for (; isspace(*str); str++) {
  }
  
  for (; *str >= '0' && *str <= '7'; str++) {
    ret *= 8;
    ret += *str - '0';
  }
  
  return ret;
}

/*
 * static int htoi
 *
 * DESCRIPTION:
 *
 * Hexadecimal string to integer translation.
 *
 * RETURNS:
 *
 * Integer converted from the string.
 *
 * ARGUMENTS:
 *
 * str - String of hexadecimal characters and digits that we are
 * converting.
 */
static	int	htoi(const char *str)
{
  int		ret = 0;
  
  /* strip off spaces */
  for (; isspace(*str); str++) {
  }
  
  /* skip a leading 0[xX] */
  if (*str == '0' && (*(str + 1) == 'x' || *(str + 1) == 'X')) {
    str += 2;
  }
  
  for (; isdigit(*str) ||
       (*str >= 'a' && *str <= 'f') || (*str >= 'A' && *str <= 'F');
       str++) {
    ret *= 16;
    if (*str >= 'a' && *str <= 'f') {
      ret += *str - 'a' + 10;
    }
    else if (*str >= 'A' && *str <= 'F') {
      ret += *str - 'A' + 10;
    }
    else {
      ret += *str - '0';
    }
  }
  
  return ret;
}

/*
 * static char *string_copy
 *
 * DESCRIPTION:
 *
 * Basically a strdup for compatibility sake.
 *
 * RETURNS:
 *
 * Character pointer that must be freed later.
 *
 * ARGUMENTS:
 *
 * str - String we are copying.
 */
static	char	*string_copy(const char *str)
{
  const char	*str_p;
  char		*copy, *copy_p;
  int		len;
  
  len = strlen(str);
  copy = (char *)malloc(len + 1);
  if (copy == NULL) {
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: memory error during argument processing\n",
		    argv_program);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return NULL;
  }
  
  for (str_p = str, copy_p = copy; *str_p != '\0';) {
    *copy_p++ = *str_p++;
  }
  *copy_p = '\0';
  
  return copy;
}

/*
 * static char **vectorize
 *
 * DESCRIPTION:
 *
 * Break a string up into its arguments separated by one of the
 * characters in a token string and return an array of char pointers.
 *
 * NOTE: the string argument should stay around until that time.
 *
 * RETURNS:
 *
 * Success - Allocated list of character poiners into the string
 * argument which must be freed later.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * str - String we are tokenizing.
 *
 * tok - List of token characters to look for in the string.
 *
 * num_tok_p - Pointer to an integer which will be set to the number
 * of tokens found in the string.
 */
static	char	**vectorize(char *str, const char *tok, int *num_tok_p)
{
  char	**vect_p;
  char	*tmp, *str_p, *tok_p;
  int	tok_c, tok_n;
  
  /* count the tokens */
  tmp = string_copy(str);
  if (tmp == NULL) {
    return NULL;
  }
  
  str_p = tmp;
  tok_c = 0;
  while (1) {
    tok_p = strsep(&str_p, tok);
    if (tok_p == NULL) {
      break;
    }
    if (*tok_p != '\0') {
      tok_c++;
    }
  }
  tok_n = tok_c;
  free(tmp);
  
  *num_tok_p = tok_n;
  
  if (tok_c == 0) {
    return NULL;
  }
  
  /* allocate the pointer grid */
  vect_p = (char **)malloc(sizeof(char *) * tok_c);
  if (vect_p == NULL) {
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: memory error during argument processing\n",
		    argv_program);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return NULL;
  }
  
  /* load the tokens into the list */
  str_p = str;
  for (tok_c = 0; tok_c < tok_n;) {
    tok_p = strsep(&str_p, tok);
    if (tok_p == NULL) {
      break;
    }
    if (*tok_p != '\0') {
      vect_p[tok_c] = tok_p;
      tok_c++;
    }
  }
  
  return vect_p;
}

/*
 * static int expand_buf
 *
 * DESCRIPTION:
 *
 * Translates a buffer of bytes into its printable version.
 *
 * NOTE: it does _not_ add a \0 at the end of OUT.
 *
 * RETURNS:
 *
 * Number of characters written in to the output buffer.
 *
 * ARGUMENTS:
 *
 * buf - Input buffer of bytes.
 *
 * buf_size - Size of the input buffer.  If < 0 then the routing will
 * translate up to the first \0.
 *
 * out - Output buffer for the translated characters.
 *
 * out_size - Maximum size of the output buffer.
 */
static	int	expand_buf(const void *buf, const int buf_size,
			   char *out, const int out_size)
{
  int			buf_c;
  const unsigned char	*buf_p, *spec_p;
  char	 		*max_p, *out_p = out;
  
  /* setup our max pointer */
  max_p = out + out_size;
  
  /* run through the input buffer, counting the characters as we go */
  for (buf_c = 0, buf_p = (const unsigned char *)buf;; buf_c++, buf_p++) {
    
    /* did we reach the end of the buffer? */
    if (buf_size < 0) {
      if (*buf_p == '\0') {
	break;
      }
    }
    else {
      if (buf_c >= buf_size) {
	break;
      }
    }
    
    /* search for special characters */
    for (spec_p = (unsigned char *)SPECIAL_CHARS + 1;
	 *(spec_p - 1) != '\0';
	 spec_p += 2) {
      if (*spec_p == *buf_p) {
	break;
      }
    }
    
    /* did we find one? */
    if (*(spec_p - 1) != '\0') {
      if (out_p + 2 >= max_p) {
	break;
      }
      (void)loc_snprintf(out_p, max_p - out_p, "\\%c", *(spec_p - 1));
      out_p += 2;
      continue;
    }
    
    /* print out any 7-bit printable characters */
    if (*buf_p < 128 && isprint(*buf_p)) {
      if (out_p + 1 >= max_p) {
	break;
      }
      *out_p = *(char *)buf_p;
      out_p += 1;
    }
    else {
      if (out_p + 4 >= max_p) {
	break;
      }
      (void)loc_snprintf(out_p, max_p - out_p, "\\%03o", *buf_p);
      out_p += 4;
    }
  }
  
  return out_p - out;
}

/****************************** usage routines *******************************/

/*
 * static void usage_short
 *
 * DESCRIPTION:
 *
 * Print a short-format usage message.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structions whose usage messages you print.
 *
 * flags - User flags.
 */
static	void	usage_short(const argv_t *args, const int flag)
{
  const argv_t	*arg_p;
  int		len, col_c = 0;
  int		mark_b = ARGV_FALSE;
  char		*prefix;
  
  if (argv_error_stream == NULL) {
    return;
  }
  
  /* print the usage message header */
  (void)fprintf(argv_error_stream, "%s%s", USAGE_LABEL, argv_program);
  col_c += USAGE_LABEL_LENGTH + strlen(argv_program);
  
  /*
   * print all of the boolean arguments first.
   * NOTE: we assume they all fit on the line
   */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    
    /* skip or-specifiers */
    if (arg_p->ar_short_arg == ARGV_OR
	|| arg_p->ar_short_arg == ARGV_XOR) {
      continue;
    }
    
    /* skip non booleans */
    if (HAS_ARG(arg_p->ar_type)) {
      continue;
    }
    
    /* skip args with no short component */
    if (arg_p->ar_short_arg == '\0') {
      continue;
    }
    
    if (! mark_b) {
      len = 2 + SHORT_PREFIX_LENGTH;
      prefix = " [";
      
      /* we check for -2 here because we should have 1 arg and ] on line */
      if (col_c + len > SCREEN_WIDTH - 2) {
	(void)fprintf(argv_error_stream, "\n%*.*s",
		      (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "");
	col_c = USAGE_LABEL_LENGTH;
	
	/* if we are the start of a line, skip any starting spaces */
	if (*prefix == ' ') {
	  prefix++;
	  len--;
	}
      }
      
      (void)fprintf(argv_error_stream, "%s%s", prefix, SHORT_PREFIX);
      col_c += len;
      mark_b = ARGV_TRUE;
    }
    
    len = 1;
    /* we check for -1 here because we should need ] */
    if (col_c + len > SCREEN_WIDTH - 1) {
      (void)fprintf(argv_error_stream, "]\n%*.*s",
		    (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "");
      col_c = USAGE_LABEL_LENGTH;
      
      /* restart the short option list */
      (void)fprintf(argv_error_stream, "[%s", SHORT_PREFIX);
      col_c += 1 + SHORT_PREFIX_LENGTH;
    }
    
    (void)fprintf(argv_error_stream, "%c", arg_p->ar_short_arg);
    col_c++;
  }
  
  if (mark_b) {
    (void)fprintf(argv_error_stream, "]");
    col_c++;
  }
  
  /* print remaining (non-boolean) arguments */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    int		var_len;
    char	*var_str, *postfix;
    
    /* skip or-specifiers */
    if (arg_p->ar_short_arg == ARGV_OR
	|| arg_p->ar_short_arg == ARGV_XOR) {
      continue;
    }
    
    /* skip booleans types */
    if (! HAS_ARG(arg_p->ar_type)) {
      continue;
    }
    
    if (arg_p->ar_var_label == NULL) {
      if (ARGV_TYPE(arg_p->ar_type) == ARGV_BOOL_ARG
	  || ARGV_TYPE(arg_p->ar_type) == ARGV_BOOL_INT_ARG) {
	var_str = BOOL_ARG_LABEL;
	var_len = BOOL_ARG_LENGTH;
      }
      else {
	var_len = UNKNOWN_ARG_LENGTH;
	var_str = UNKNOWN_ARG;
      }
    }
    else {
      var_len = strlen(arg_p->ar_var_label);
      var_str = arg_p->ar_var_label;
    }
    
    if (arg_p->ar_short_arg == ARGV_MAND) {
      /* print the mandatory argument desc */
      len = 1 + var_len;
      prefix = " ";
      postfix = "";
    }
    else if (arg_p->ar_short_arg == ARGV_MAYBE) {
      /* print the maybe argument desc */      
      len = 2 + var_len + 1;
      prefix = " [";
      postfix = "]";
    }
    else {
      /* handle options with arguments */
      
      /* " [" + short_prefix + char */
      len = 2 + SHORT_PREFIX_LENGTH + 1;
      prefix = " [";
      
      /* do we need to wrap */
      if (col_c + len > SCREEN_WIDTH) {
	(void)fprintf(argv_error_stream, "\n%*.*s",
		      (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "");
	col_c = USAGE_LABEL_LENGTH;
	
	/* if we are the start of a line, skip any starting spaces */
	if (*prefix == ' ') {
	  prefix++;
	  len--;
	}
      }
      (void)fprintf(argv_error_stream, "%s%s%c",
		    prefix, SHORT_PREFIX, arg_p->ar_short_arg);
      col_c += len;
      
      len = 1 + var_len + 1;
      prefix = " ";
      postfix = "]";
    }
    
    if (col_c + len > SCREEN_WIDTH) {
      (void)fprintf(argv_error_stream, "\n%*.*s",
		    (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "");
      col_c = USAGE_LABEL_LENGTH;
      
      /* if we are the start of a line, skip any starting spaces */
      if (*prefix == ' ') {
	prefix++;
	len--;
      }
    }
    
    (void)fprintf(argv_error_stream, "%s%s%s", prefix, var_str, postfix);
    col_c += len;
  }
  
  (void)fprintf(argv_error_stream, "\n");
  
  if (flag == ARGV_USAGE_SHORT_REM) {
    (void)fprintf(argv_error_stream,
		  "%*.*sUse the '%s%s' argument for more assistance.\n",
		  (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "",
		  LONG_PREFIX, USAGE_ARG);
  }
}

/*
 * static void display_arg
 *
 * DESCRIPTION:
 *
 * Display an argument type while keeping track of the column we are
 * in.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * stream - Output stream we are writing to.
 *
 * arg_p - Argument that we are displaying.
 *
 * max - Maximum column position to write to.
 *
 * col_cp - Pointer to an integer to record the column position.
 */
static	void	display_arg(FILE *stream, const argv_t *arg_p, const int max,
			    int *col_cp)
{
  int	var_len, len;
  
  if (arg_p->ar_var_label == NULL) {
    var_len = 0;
  }
  else {
    var_len = strlen(arg_p->ar_var_label);
  }
  
  switch (ARGV_TYPE(arg_p->ar_type)) {
    
  case ARGV_BOOL:
  case ARGV_BOOL_NEG:
  case ARGV_INCR:
  case ARGV_BOOL_INT:
  case ARGV_BOOL_INT_NEG:
    break;
    
  case ARGV_BOOL_ARG:
  case ARGV_BOOL_INT_ARG:
    (void)fprintf(stream, "%s", BOOL_ARG_LABEL);
    (*col_cp) += BOOL_ARG_LENGTH;
    break;
    
  case ARGV_CHAR:
  case ARGV_CHAR_P:
  case ARGV_SHORT:
  case ARGV_U_SHORT:
  case ARGV_INT:
  case ARGV_U_INT:
  case ARGV_LONG:
  case ARGV_U_LONG:
  case ARGV_FLOAT:
  case ARGV_DOUBLE:
  case ARGV_BIN:
  case ARGV_OCT:
  case ARGV_HEX:
  case ARGV_SIZE:
  case ARGV_U_SIZE:
    if (arg_p->ar_var_label == NULL) {
      len = max - *col_cp;
      (void)fprintf(stream, "%-.*s", len, UNKNOWN_ARG);
      *col_cp += MIN(len, (int)UNKNOWN_ARG_LENGTH);
    }
    else {
      len = max - *col_cp;
      (void)fprintf(stream, "%-.*s", len, arg_p->ar_var_label);
      *col_cp += MIN(len, var_len);
    }
    break;
  }
}

/*
 * static void display_option
 *
 * DESCRIPTION:
 *
 * Display an option entry while while keeping track of the column we
 * are in.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * stream - Output stream we are writing to.
 *
 * arg_p - Argument that we are displaying.
 *
 * max - Maximum column position to write to.
 *
 * col_cp - Pointer to an integer to record the column position.
 */
static	void	display_option(FILE *stream, const argv_t *arg_p, int *col_cp)
{
  if (stream == NULL) {
    return;
  }
  
  (void)fputc('[', stream);
  (*col_cp)++;
  
  /* arg maybe does not have a -? preface */
  if (arg_p->ar_short_arg != ARGV_MAYBE) {
    (void)fprintf(stream, "%s%c",
		  SHORT_PREFIX, arg_p->ar_short_arg);
    *col_cp += SHORT_PREFIX_LENGTH + 1;
    
    if (HAS_ARG(arg_p->ar_type)) {
      /* display optional argument */
      (void)fputc(' ', stream);
      (*col_cp)++;
    }
  }
  
  display_arg(stream, arg_p, LONG_COLUMN - 1, col_cp);
  (void)fputc(']', stream);
  (*col_cp)++;
}

/*
 * static void usage_long
 *
 * DESCRIPTION:
 *
 * Print a long-format usage message.
 *
 * RETURNS:
 *
 * None.
 *
 * ars - Array of argv_t structures whose usage we are printing. 
 */
static	void	usage_long(const argv_t *args)
{
  const argv_t	*arg_p;
  int		col_c, len;
  
  if (argv_error_stream == NULL) {
    return;
  }
  
  /* print the usage message header */
  (void)fprintf(argv_error_stream, "%s%s\n", USAGE_LABEL, argv_program);
  
  /* run through the argument structure */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    
    /* skip or specifiers */
    if (arg_p->ar_short_arg == ARGV_OR || arg_p->ar_short_arg == ARGV_XOR) {
      continue;
    }
    
    /* indent to the short-option col_c */
    (void)fprintf(argv_error_stream, "%*.*s", SHORT_COLUMN, SHORT_COLUMN, "");
    
    /* start column counter */
    col_c = SHORT_COLUMN;
    
    /* print the short-arg stuff if there */
    if (arg_p->ar_short_arg == '\0') {
      (void)fputc('[', argv_error_stream);
      col_c++;
    }
    else {
      if (arg_p->ar_short_arg == '\0') {
	;
      }
      else if (arg_p->ar_short_arg == ARGV_MAND) {
	display_arg(argv_error_stream, arg_p, COMMENT_COLUMN, &col_c);
      }
      else {
	/* ARGV_MAYBE handled here */
	display_option(argv_error_stream, arg_p, &col_c);
      }
      
      /* put the long-option message on the correct column */
      if (col_c < LONG_COLUMN) {
	(void)fprintf(argv_error_stream, "%*.*s",
		      LONG_COLUMN - col_c, LONG_COLUMN - col_c, "");
	col_c = LONG_COLUMN;
      }
    }
    
    /* print the long-option message */
    if (arg_p->ar_long_arg != NULL) {
      len = COMMENT_COLUMN - col_c - (LONG_PREFIX_LENGTH + 1);
      if (arg_p->ar_short_arg != '\0') {
	(void)fprintf(argv_error_stream, "%s", LONG_LABEL);
	col_c += LONG_LABEL_LENGTH;
	len -= LONG_LABEL_LENGTH;
      }
      (void)fprintf(argv_error_stream, "%s%-.*s",
		    LONG_PREFIX, len, arg_p->ar_long_arg);
      col_c += LONG_PREFIX_LENGTH + MIN(len, (int)strlen(arg_p->ar_long_arg));
    }
    
    /* add the optional argument if no short-arg */
    if (arg_p->ar_short_arg == '\0') {
      if (HAS_ARG(arg_p->ar_type)) {
	(void)fputc(' ', argv_error_stream);
	col_c++;
      }
      
      /* display any optional arguments */
      display_arg(argv_error_stream, arg_p, COMMENT_COLUMN - 1, &col_c);
      (void)fputc(']', argv_error_stream);
      col_c++;
    }
    
    /* print the comment */
    if (arg_p->ar_comment != NULL) {
      /* put the comment message on the correct column */
      if (col_c < COMMENT_COLUMN) {
	(void)fprintf(argv_error_stream, "%*.*s",
		      COMMENT_COLUMN - col_c,
		      COMMENT_COLUMN - col_c, "");
	col_c = COMMENT_COLUMN;
      }
      
      len = SCREEN_WIDTH - col_c - COMMENT_LABEL_LENGTH;
      (void)fprintf(argv_error_stream, "%s%-.*s",
		    COMMENT_LABEL, len, arg_p->ar_comment);
    }
    
    (void)fprintf(argv_error_stream, "\n");
  }
}

/*
 * static void do_usage
 *
 * DESCRIPTION:
 *
 * Print the usage messages.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures.
 *
 * flag - Users flags which will tell us whether to display short or
 * long usage messages.
 */
static	void	do_usage(const argv_t *args, const int flag)
{
  if (argv_error_stream == NULL) {
    return;
  }
  
  if (flag == ARGV_USAGE_SEE) {
    (void)fprintf(argv_error_stream,
		  "%*.*sUse the '%s%s' argument for assistance.\n",
		  (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "",
		  LONG_PREFIX, USAGE_ARG);
  }
  else if (flag == ARGV_USAGE_SHORT || flag == ARGV_USAGE_SHORT_REM) {
    usage_short(args, flag);
  }
  else if (flag == ARGV_USAGE_LONG || flag == ARGV_USAGE_ALL) {
    usage_long(args);
  }
  
  if (flag == ARGV_USAGE_ALL) {
    (void)fprintf(argv_error_stream, "\n");
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' for default usage information.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, USAGE_ARG);
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' for short usage information.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, USAGE_SHORT_ARG);
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' for long usage information.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, USAGE_LONG_ARG);
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' for all usage information.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, USAGE_ALL_ARG);
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' to display the help message.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, HELP_ARG);
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' to display the version message.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, VERSION_ARG);
    (void)fprintf(argv_error_stream,
		  "%*.*sUse '%s%s' to display the options and their values.\n",
		  SHORT_COLUMN, SHORT_COLUMN, "",
		  LONG_PREFIX, DISPLAY_ARG);
  }
}

/******************************* preprocessing *******************************/

/*
 * static int preprocess_array
 *
 * DESCRIPTION:
 *
 * Preprocess argument array entries and set the mandatory and maybe
 * flags.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures.
 *
 * arg_n - Number of entries in the argv_t array.  We need this for a
 * couple of reasons.
 */
static	int	preprocess_array(argv_t *args, const int arg_n)
{
  argv_t	*arg_p;
  int		mand_array_b = ARGV_FALSE, maybe_field_b = ARGV_FALSE;
  
  /* count the args and find the first mandatory */
  for (arg_p = args; arg_p < args + arg_n; arg_p++) {
    
    /* clear internal flags */
    arg_p->ar_type &= ~ARGV_FLAG_USED;
    
    /* do we have a mandatory-array? */
    if (arg_p->ar_short_arg == ARGV_MAND) {
      if (mand_array_b) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, no ARGV_MAND's can follow a MAND or MAYBE array\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
      if (maybe_field_b) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, no ARGV_MAND's can follow a ARGV_MAYBE\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
      
      if (arg_p->ar_type & ARGV_FLAG_ARRAY) {
	mand_array_b = ARGV_TRUE;
      }
    }
    
    /* do we have a maybe field? */
    if (arg_p->ar_short_arg == ARGV_MAYBE) {
      if (mand_array_b) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, no ARGV_MAYBE's can follow a MAND or MAYBE array\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
      
      maybe_field_b = ARGV_TRUE;
      if (arg_p->ar_type & ARGV_FLAG_ARRAY) {
	mand_array_b = ARGV_TRUE;
      }
    }
    
    /* handle initializing the argument array */
    if (arg_p->ar_type & ARGV_FLAG_ARRAY) {
      argv_array_t	*arrp = (argv_array_t *)arg_p->ar_variable;
      
      if (! HAS_ARG(arg_p->ar_type)) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, cannot have an array of boolean values\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
      if (ARGV_TYPE(arg_p->ar_type) == ARGV_INCR) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, cannot have an array of incremental values\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
      arrp->aa_entry_n = 0;
    }
    
    /* verify variable pointer */
    if (arg_p->ar_variable == NULL
	&& arg_p->ar_short_arg != ARGV_OR
	&& arg_p->ar_short_arg != ARGV_XOR) {
      if (argv_error_stream != NULL) {
	(void)fprintf(argv_error_stream,
		      "%s: %s, NULL variable specified in arg array\n",
		      argv_program, INTERNAL_ERROR_NAME);
      }
      if (argv_interactive) {
	(void)exit(argv_error_code);
      }
      return ERROR;
    }
    
    /* verify [X]OR's */
    if (arg_p->ar_short_arg == ARGV_OR
	|| arg_p->ar_short_arg == ARGV_XOR) {
      
      /* that they are not at the start or end of list */
      if (arg_p == args || arg_p >= (args + arg_n - 1)) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, ARGV_[X]OR entries cannot be at start or end of array\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
      
      /* that two aren't next to each other */
      if ((arg_p - 1)->ar_short_arg == ARGV_OR
	  || (arg_p - 1)->ar_short_arg == ARGV_XOR) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, two ARGV_[X]OR entries cannot be next to each other\n",
			argv_program, INTERNAL_ERROR_NAME);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return ERROR;
      }
    }
  }
  
  return NOERROR;
}

/*
 * static int string_to_value
 *
 * DESCRIPTION:
 *
 * Translate string value argument into a variable value depending on
 * its type.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * arg - Argument string.
 *
 * var - Pointer to our variable.
 *
 * type - Type of the variable.
 */
static	int	string_to_value(const char *arg, ARGV_PNT var,
				const unsigned int type)
{
  argv_array_t	*arr_p;
  argv_type_t	*type_p;
  unsigned int	val_type = ARGV_TYPE(type), size = 0;
  
  /* find the type and the size for array */
  for (type_p = argv_types; type_p->at_value != 0; type_p++) {
    if (type_p->at_value == val_type) {
      size = type_p->at_size;
      break;
    }
  }
  
  if (type_p->at_value == 0) {
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream, "%s: illegal variable type %d\n",
		    __FILE__, val_type);
    }
    return ERROR;
  }
  
  if (type & ARGV_FLAG_ARRAY) {
    arr_p = (argv_array_t *)var;
    
    if (arr_p->aa_entry_n == 0) {
      arr_p->aa_entries = (char *)malloc(ARRAY_INCR *size);
    }
    else if (arr_p->aa_entry_n % ARRAY_INCR == 0) {
      arr_p->aa_entries =
	(char *)realloc(arr_p->aa_entries, (arr_p->aa_entry_n + ARRAY_INCR) *
			size);
    }
    
    if (arr_p->aa_entries == NULL) {
      if (argv_error_stream != NULL) {
	(void)fprintf(argv_error_stream,
		      "%s: memory error during argument processing\n",
		      argv_program);
      }
      if (argv_interactive) {
	(void)exit(argv_error_code);
      }
      return ERROR;
    }
    
    var = (char *)(arr_p->aa_entries) + arr_p->aa_entry_n * size;
    arr_p->aa_entry_n++;
  }
  
  /* translate depending on type */
  switch (val_type) {
    
  case ARGV_BOOL:
    /* if no close argument, set to true */
    if (arg == NULL) {
      *(char *)var = ARGV_TRUE;
    }
    else if (*(char *)arg == 't' || *(char *)arg == 'T'
	     || *(char *)arg == 'y' || *(char *)arg == 'Y'
	     || *(char *)arg == '1') {
      *(char *)var = ARGV_TRUE;
    }
    else {
      *(char *)var = ARGV_FALSE;
    }
    break;
    
  case ARGV_BOOL_NEG:
    /* if no close argument, set to false */
    if (arg == NULL) {
      *(char *)var = ARGV_FALSE;
    }
    else if (*(char *)arg == 't' || *(char *)arg == 'T'
	     || *(char *)arg == 'y' || *(char *)arg == 'Y'
	     || *(char *)arg == '1') {
      *(char *)var = ARGV_TRUE;
    }
    else {
      *(char *)var = ARGV_FALSE;
    }
    break;
    
  case ARGV_BOOL_ARG:
    if (*(char *)arg == 't' || *(char *)arg == 'T'
	|| *(char *)arg == 'y' || *(char *)arg == 'Y'
	|| *(char *)arg == '1') {
      *(char *)var = ARGV_TRUE;
    }
    else {
      *(char *)var = ARGV_FALSE;
    }
    break;
    
  case ARGV_CHAR:
    *(char *)var = *(char *)arg;
    break;
    
  case ARGV_CHAR_P:
    *(char **)var = string_copy((char *)arg);
    if (*(char **)var == NULL) {
      return ERROR;
    }
    break;
    
  case ARGV_SHORT:
    *(short *)var = (short)atoi(arg);
    break;
    
  case ARGV_U_SHORT:
    *(unsigned short *)var = (unsigned short)atoi(arg);
    break;
    
  case ARGV_INT:
    *(int *)var = atoi(arg);
    break;
    
  case ARGV_U_INT:
    *(unsigned int *)var = atoi(arg);
    break;
    
  case ARGV_LONG:
    *(long *)var = atol(arg);
    break;
    
  case ARGV_U_LONG:
    *(unsigned long *)var = atol(arg);
    break;
    
  case ARGV_FLOAT:
    (void)sscanf(arg, "%f", (float *)var);
    break;
    
  case ARGV_DOUBLE:
    (void)sscanf(arg, "%lf", (double *)var);
    break;
    
  case ARGV_BIN:
    *(int *)var = btoi(arg);
    break;
    
  case ARGV_OCT:
    *(int *)var = otoi(arg);
    break;
    
  case ARGV_HEX:
    *(int *)var = htoi(arg);
    break;
    
  case ARGV_INCR:
    /* if no close argument then increment else set the value */
    if (arg == NULL) {
      (*(int *)var)++;
    }
    else {
      *(int *)var = atoi(arg);
    }
    break;
    
  case ARGV_SIZE:
    {
      const char	*arg_p;
      long		val;
      
      /* take initial integer point */
      val = atol(arg);
      for (arg_p = arg;
	   *arg_p == ' ' || *arg_p == '-' || *arg_p == '+'
	     || (*arg_p >= '0' && *arg_p <= '9');
	   arg_p++) {
      }
      if (*arg_p == 'b' || *arg_p == 'B') {
	val *= 1;
      }
      else if (*arg_p == 'k' || *arg_p == 'B') {
	val *= 1024;
      }
      else if (*arg_p == 'm' || *arg_p == 'M') {
	val *= 1024 * 1024;
      }
      else if (*arg_p == 'g' || *arg_p == 'G') {
	val *= 1024 * 1024 * 1024;
      }
      *(long *)var = val;
    }
    break;
    
  case ARGV_U_SIZE:
    {
      const char	*arg_p;
      unsigned long	val;
      
      /* take initial integer point */
      val = (unsigned long)atol(arg);
      for (arg_p = arg;
	   *arg_p == ' ' || *arg_p == '-' || *arg_p == '+'
	     || (*arg_p >= '0' && *arg_p <= '9');
	   arg_p++) {
      }
      if (*arg_p == 'b' || *arg_p == 'B') {
	val *= 1;
      }
      else if (*arg_p == 'k' || *arg_p == 'B') {
	val *= 1024;
      }
      else if (*arg_p == 'm' || *arg_p == 'M') {
	val *= 1024 * 1024;
      }
      else if (*arg_p == 'g' || *arg_p == 'G') {
	val *= 1024 * 1024 * 1024;
      }
      *(unsigned long *)var = val;
    }
    break;
    
  case ARGV_BOOL_INT:
    /* if no close argument, set to true */
    if (arg == NULL) {
      *(int *)var = ARGV_TRUE;
    }
    else if (*(char *)arg == 't' || *(char *)arg == 'T'
	     || *(char *)arg == 'y' || *(char *)arg == 'Y'
	     || *(char *)arg == '1') {
      *(int *)var = ARGV_TRUE;
    }
    else {
      *(int *)var = ARGV_FALSE;
    }
    break;
    
  case ARGV_BOOL_INT_NEG:
    /* if no close argument, set to false */
    if (arg == NULL) {
      *(int *)var = ARGV_FALSE;
    }
    else if (*(char *)arg == 't' || *(char *)arg == 'T'
	     || *(char *)arg == 'y' || *(char *)arg == 'Y'
	     || *(char *)arg == '1') {
      *(int *)var = ARGV_TRUE;
    }
    else {
      *(int *)var = ARGV_FALSE;
    }
    break;
    
  case ARGV_BOOL_INT_ARG:
    if (*(char *)arg == 't' || *(char *)arg == 'T'
	|| *(char *)arg == 'y' || *(char *)arg == 'Y'
	|| *(char *)arg == '1') {
      *(int *)var = ARGV_TRUE;
    }
    else {
      *(int *)var = ARGV_FALSE;
    }
    break;
    
  }
  
  return NOERROR;
}

/*
 * static int value_to_string
 *
 * DESCRIPTION:
 *
 * Translate value from variable depending on its type intoits string
 * represetnation in buffer.
 *
 * RETURNS:
 *
 * Number of characters added to the buffer.
 *
 * ARGUMENTS:
 *
 * var - Variable pointer.
 *
 * type - Type of variable.
 *
 * buf - User buffer to convert into.
 *
 * buf_size - Size of the user buffer.
 */
static	int	value_to_string(const ARGV_PNT var, const unsigned int type,
				char *buf, const int buf_size)
{
  int	len = 0;
  
  /*
   * NOTE: without a snprintf, we have to hope that buf_size > integer
   * and the string repesentations of the numbers.
   */ 
  
  /* translate depending on type */
  switch (ARGV_TYPE(type)) {
    
  case ARGV_BOOL:
  case ARGV_BOOL_NEG:
  case ARGV_BOOL_ARG:
    if (*(char *)var) {
      strncpy(buf, "true (! 0)", buf_size);
    }
    else {
      strncpy(buf, "false (0)", buf_size);
    }
    buf[buf_size - 1] = '\0';
    len = strlen(buf);
    break;
    
  case ARGV_CHAR:
    len = expand_buf((char *)var, 1, buf, buf_size);
    break;
    
  case ARGV_CHAR_P:
    if (*(char **)var == NULL) {
      strncpy(buf, "(null)", buf_size);
      buf[buf_size - 1] = '\0';
      len = strlen(buf);
    }
    else {
      len = expand_buf(*(char **)var, -1, buf, buf_size);
    }
    break;
    
  case ARGV_SHORT:
    (void)loc_snprintf(buf, buf_size, "%d", *(short *)var);
    len = strlen(buf);
    break;
    
  case ARGV_U_SHORT:
    (void)loc_snprintf(buf, buf_size, "%d", *(unsigned short *)var);
    len = strlen(buf);
    break;
    
  case ARGV_INT:
    (void)loc_snprintf(buf, buf_size, "%d", *(int *)var);
    len = strlen(buf);
    break;
    
  case ARGV_U_INT:
    (void)loc_snprintf(buf, buf_size, "%u", *(unsigned int *)var);
    len = strlen(buf);
    break;
    
  case ARGV_LONG:
    (void)loc_snprintf(buf, buf_size, "%ld", *(long *)var);
    len = strlen(buf);
    break;
    
  case ARGV_U_LONG:
    (void)loc_snprintf(buf, buf_size, "%lu", *(unsigned long *)var);
    len = strlen(buf);
    break;
    
  case ARGV_FLOAT:
    (void)loc_snprintf(buf, buf_size, "%f", *(float *)var);
    len = strlen(buf);
    break;
    
  case ARGV_DOUBLE:
    (void)loc_snprintf(buf, buf_size, "%f", *(double *)var);
    len = strlen(buf);
    break;
    
    /* this should be a routine */
  case ARGV_BIN:
    {
      int	bit_c, bit, first_b = ARGV_FALSE;
      char	binary[2 + 128 + 1], *bin_bounds_p, *bin_p = binary;
      
      if (*(int *)var == 0) {
	strncpy(buf, "0", buf_size);
      }
      else {
	
	bin_bounds_p = binary + sizeof(binary);
	
	/* initially write binary number into tmp buffer, then copy into out */
	*bin_p++ = '0';
	*bin_p++ = 'b';
	
	for (bit_c = sizeof(int) * BITS_IN_BYTE - 1; bit_c >= 0; bit_c--) {
	  bit = *(int *)var & (1 << bit_c);
	  
	  if (bit == 0) {
	    if (first_b) {
	      *bin_p++ = '0';
	    }
	  }
	  else {
	    *bin_p++ = '1';
	    first_b = ARGV_TRUE;
	  }
	}
	
	/* add on the decimal equivalent */ 
	(void)loc_snprintf(bin_p, bin_bounds_p - bin_p, " (%d)", *(int *)var);
	/* find the \0 at end */ 
	for (; *bin_p != '\0'; bin_p++) {
	}
	
	/* now we copy from the binary buffer to the output */
	strncpy(buf, binary, buf_size);
      }
      
      buf[buf_size - 1] = '\0';
      len = strlen(buf);
    }
    break;
    
  case ARGV_OCT:
    if (*(int *)var == 0) {
      (void)strncpy(buf, "0", buf_size);
      buf[buf_size - 1] = '\0';
    }
    else {
      (void)loc_snprintf(buf, buf_size, "%#o (%d)", *(int *)var, *(int *)var);
    }
    len = strlen(buf);
    break;
    
  case ARGV_HEX:
    if (*(int *)var == 0) {
      (void)strcpy(buf, "0");
    }
    else {
      (void)loc_snprintf(buf, buf_size, "%#x (%d)", *(int *)var, *(int *)var);
    }
    len = strlen(buf);
    break;
    
  case ARGV_INCR:
    (void)loc_snprintf(buf, buf_size, "%d", *(int *)var);
    len = strlen(buf);
    break;
    
  case ARGV_SIZE:
    {
      long	morf, val = *(long *)var;
      
      if (val == 0) {
	(void)strcpy(buf, "0");
      }
      else if (val % (1024 * 1024 * 1024) == 0) {
	morf = val / (1024 * 1024 * 1024);
	(void)loc_snprintf(buf, buf_size, "%ldg (%ld)", morf, val);
      }
      else if (val % (1024 * 1024) == 0) {
	morf = val / (1024 * 1024);
	(void)loc_snprintf(buf, buf_size, "%ldm (%ld)", morf, val);
      }
      else if (val % 1024 == 0) {
	morf = val / 1024;
	(void)loc_snprintf(buf, buf_size, "%ldk (%ld)", morf, val);
      }
      else {
	(void)loc_snprintf(buf, buf_size, "%ld", val);
      }
      
      len = strlen(buf);
    }
    break;
    
  case ARGV_U_SIZE:
    {
      unsigned long	morf, val = *(unsigned long *)var;
      
      if (val == 0) {
	(void)strcpy(buf, "0");
      }
      else if (val % (1024 * 1024 * 1024) == 0) {
	morf = val / (1024 * 1024 * 1024);
	(void)loc_snprintf(buf, buf_size, "%ldg (%ld)", morf, val);
      }
      else if (val % (1024 * 1024) == 0) {
	morf = val / (1024 * 1024);
	(void)loc_snprintf(buf, buf_size, "%ldm (%ld)", morf, val);
      }
      else if (val % 1024 == 0) {
	morf = val / 1024;
	(void)loc_snprintf(buf, buf_size, "%ldk (%ld)", morf, val);
      }
      else {
	(void)loc_snprintf(buf, buf_size, "%ld", val);
      }
      
      len = strlen(buf);
    }
    break;
    
  case ARGV_BOOL_INT:
  case ARGV_BOOL_INT_NEG:
  case ARGV_BOOL_INT_ARG:
    if (*(int *)var) {
      strncpy(buf, "true (! 0)", buf_size);
    }
    else {
      strncpy(buf, "false (0)", buf_size);
    }
    buf[buf_size - 1] = '\0';
    len = strlen(buf);
    break;
    
  default:
    strncpy(buf, "(unknown)", buf_size);
    buf[buf_size - 1] = '\0';
    len = strlen(buf);
    break;
  }
  
  return len;
}

/*
 * static void display_variables
 *
 * DESCRIPTION:
 *
 * Display all of the variable values from our array.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures whose variables we are
 * displaying.
 */
static	void	display_variables(const argv_t *args)
{
  const argv_t	*arg_p;
  argv_type_t	*type_p;
  char		buf[256];
  int		len, col_c;
  unsigned int	val_type;
  
  /* run through the argument structure */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    
    val_type = ARGV_TYPE(arg_p->ar_type);
    
    /* skip or specifiers */
    if (arg_p->ar_short_arg == ARGV_OR || arg_p->ar_short_arg == ARGV_XOR) {
      continue;
    }
    
    col_c = 0;
    if (arg_p->ar_short_arg == '\0') {
      if (arg_p->ar_long_arg != NULL) {
	len = COMMENT_COLUMN - col_c - (LONG_PREFIX_LENGTH + 1);
	if (arg_p->ar_short_arg != '\0') {
	  (void)fprintf(argv_error_stream, "%s", LONG_LABEL);
	  col_c += LONG_LABEL_LENGTH;
	  len -= LONG_LABEL_LENGTH;
	}
	(void)fprintf(argv_error_stream, "%s%-.*s",
		      LONG_PREFIX, len, arg_p->ar_long_arg);
	col_c += LONG_PREFIX_LENGTH + MIN(len,
					  (int)strlen(arg_p->ar_long_arg));
      }
    }
    else if (arg_p->ar_short_arg == ARGV_MAND) {
      display_arg(argv_error_stream, arg_p, COMMENT_COLUMN, &col_c);
    }
    else {
      /* ARGV_MAYBE handled here */
      display_option(argv_error_stream, arg_p, &col_c);
    }
    
    /* put the type in the correct column */
    if (col_c < LONG_COLUMN) {
      (void)fprintf(argv_error_stream, "%*.*s",
		    LONG_COLUMN - col_c, LONG_COLUMN - col_c, "");
      col_c = LONG_COLUMN;
    }
    
    /* find the type */
    type_p = NULL;
    for (type_p = argv_types; type_p->at_value != 0; type_p++) {
      if (type_p->at_value == ARGV_TYPE(arg_p->ar_type)) {
	int	tlen;
	
	len = COMMENT_COLUMN - col_c - 1;
	tlen = strlen(type_p->at_name);
	(void)fprintf(argv_error_stream, " %-.*s", len, type_p->at_name);
	col_c += MIN(len, tlen);
	if (arg_p->ar_type & ARGV_FLAG_ARRAY) {
	  (void)fprintf(argv_error_stream, "%s", ARRAY_LABEL);
	  col_c += sizeof(ARRAY_LABEL) - 1;
	}
	break;
      }
    }
    
    if (col_c < COMMENT_COLUMN) {
      (void)fprintf(argv_error_stream, "%*.*s",
		    COMMENT_COLUMN - col_c, COMMENT_COLUMN - col_c, "");
      col_c = COMMENT_COLUMN;
    }
    
    if (arg_p->ar_type & ARGV_FLAG_ARRAY) {
      argv_array_t	*arr_p;
      int		entry_c, size = 0;
      
      /* find the type and the size for array */
      if (type_p == NULL) {
	(void)fprintf(argv_error_stream, "%s: illegal variable type %d\n",
		      __FILE__, val_type);
	continue;
      }
      size = type_p->at_size;
      arr_p = (argv_array_t *)arg_p->ar_variable;
      
      if (arr_p->aa_entry_n == 0) {
	(void)fprintf(argv_error_stream, "no entries");
      }
      else {
	for (entry_c = 0; entry_c < arr_p->aa_entry_n; entry_c++) {
	  ARGV_PNT	var;
	  if (entry_c > 0) {
	    (void)fputc(',', argv_error_stream);
	  }
	  var = (char *)(arr_p->aa_entries) + entry_c * size;
	  len = value_to_string(var, val_type, buf, sizeof(buf));
	  (void)fwrite(buf, sizeof(char), len, argv_error_stream);
	}
      }
    }
    else {
      len = value_to_string(arg_p->ar_variable, val_type, buf, sizeof(buf));
      (void)fwrite(buf, sizeof(char), len, argv_error_stream);
    }
    (void)fputc('\n', argv_error_stream);
  }
}

/************************** checking used arguments **************************/

/*
 * static int check_or
 *
 * DESCRIPTION:
 *
 * Check out if an argument has an ARGV_OR attached to it and both
 * variables have not been set.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures that we are checking.
 *
 * which_p - Pointer to the specific argument that we are checking for
 * the ARGV_OR.
 */
static	int	check_or(const argv_t *args, const argv_t *which_p)
{
  const argv_t	*arg_p, *match_p = NULL;
  
  /* check ORs below */
  for (arg_p = which_p - 2; arg_p >= args; arg_p -= 2) {
    if ((arg_p + 1)->ar_short_arg != ARGV_OR
	&& (arg_p + 1)->ar_short_arg != ARGV_XOR) {
      break;
    }
    if (arg_p->ar_type & ARGV_FLAG_USED) {
      match_p = arg_p;
      break;
    }
  }
  
  /* check ORs above */
  if (match_p == NULL) {
    /* NOTE: we assume that which_p is not pointing now to ARGV_LAST */
    for (arg_p = which_p + 2;
	 arg_p->ar_short_arg != ARGV_LAST
	 && (arg_p - 1)->ar_short_arg != ARGV_LAST;
	 arg_p += 2) {
      if ((arg_p - 1)->ar_short_arg != ARGV_OR
	  && (arg_p - 1)->ar_short_arg != ARGV_XOR) {
	break;
      }
      if (arg_p->ar_type & ARGV_FLAG_USED) {
	match_p = arg_p;
	break;
      }
    }
  }
  
  /* did we not find a problem? */
  if (match_p == NULL) {
    return NOERROR;
  }
  
  if (argv_error_stream == NULL) {
    return ERROR;
  }
  
  (void)fprintf(argv_error_stream,
		"%s: %s, specify only one of the following:\n",
		argv_program, USAGE_ERROR_NAME);
  
  /* little hack to print the one that matched and the one we were checking */
  for (;;) {
    if (match_p->ar_long_arg == NULL) {
      (void)fprintf(argv_error_stream, "%*.*s%s%c\n",
		    (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "",
		    SHORT_PREFIX, match_p->ar_short_arg);
    }
    else {
      (void)fprintf(argv_error_stream, "%*.*s%s%c (%s%s)\n",
		    (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "",
		    SHORT_PREFIX, match_p->ar_short_arg,
		    LONG_PREFIX, match_p->ar_long_arg);
    }
    
    if (match_p == which_p) {
      break;
    }
    match_p = which_p;
  }
  
  return ERROR;
}

/*
 * static int check_xor
 *
 * DESCRIPTION:
 *
 * Check out if an argument has an ARGV_XOR attached to it and that at
 * least one but not both variables have been set.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures that we are checking.
 *
 * which_p - Pointer to the specific argument that we are checking for
 * the ARGV_XOR.
 */
static	int	check_xor(const argv_t *args)
{
  const argv_t	*start_p = NULL, *arg_p;
  
  /* run through the list of arguments */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    
    /* only check the XORs */
    if (arg_p->ar_short_arg != ARGV_XOR) {
      continue;
    }
    
    start_p = arg_p;
    
    /*
     * NOTE: we are guaranteed that we are on a XOR so there is
     * something below and above...
     */
    if ((arg_p - 1)->ar_type & ARGV_FLAG_USED) {
      start_p = NULL;
    }
    
    /* run through all XORs */
    for (;;) {
      arg_p++;
      if (arg_p->ar_type & ARGV_FLAG_USED) {
	start_p = NULL;
      }
      if ((arg_p + 1)->ar_short_arg != ARGV_XOR) {
	break;
      }
      arg_p++;
    }
    
    /* were none of the xor's filled? */
    if (start_p != NULL) {
      break;
    }
  }
  
  /* did we not find a problem? */
  if (start_p == NULL) {
    return NOERROR;
  }
  
  /* arg_p points to the first XOR which failed */
  if (argv_error_stream == NULL) {
    return ERROR;
  }
  
  (void)fprintf(argv_error_stream, "%s: %s, must specify one of:\n",
		argv_program, USAGE_ERROR_NAME);
  
  for (arg_p = start_p;; arg_p += 2) {
    /*
     * NOTE: we are guaranteed that we are on a XOR so there is
     * something below and above...
     */
    (void)fprintf(argv_error_stream, "%*.*s%s%c",
		  (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "",
		  SHORT_PREFIX, (arg_p - 1)->ar_short_arg);
    if ((arg_p - 1)->ar_long_arg != NULL) {
      (void)fprintf(argv_error_stream, " (%s%s)",
		    LONG_PREFIX, (arg_p - 1)->ar_long_arg);
    }
    (void)fprintf(argv_error_stream, "\n");
    
    if (arg_p->ar_short_arg != ARGV_XOR) {
      break;
    }
  }
  
  return ERROR;
}

/*
 * static int check_mand
 *
 * DESCRIPTION:
 *
 * Verify that all of the mandatory arguments in our array have been
 * specified.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures that we are checking.
 */
static	int	check_mand(const argv_t *args)
{
  const argv_t	*arg_p;
  int		mand_c = 0, flag_c = 0;
  
  /* see if there are any mandatory args left */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    if (arg_p->ar_short_arg == ARGV_MAND
	&& (! (arg_p->ar_type & ARGV_FLAG_USED))) {
      mand_c++;
    }
    if (arg_p->ar_type & ARGV_FLAG_MAND
	&& (! (arg_p->ar_type & ARGV_FLAG_USED))) {
      flag_c++;
      if (argv_error_stream != NULL) {
	if (flag_c == 1) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, these mandatory flags must be specified:\n",
			argv_program, USAGE_ERROR_NAME);
	}
	(void)fprintf(argv_error_stream, "%*.*s%s%c",
		      (int)USAGE_LABEL_LENGTH, (int)USAGE_LABEL_LENGTH, "",
		      SHORT_PREFIX, arg_p->ar_short_arg);
	if (arg_p->ar_long_arg != NULL) {
	  (void)fprintf(argv_error_stream, " (%s%s)",
			LONG_PREFIX, arg_p->ar_long_arg);
	}
	(void)fprintf(argv_error_stream, "\n");
      }
    }
  }
  
  if (mand_c > 0 && argv_error_stream != NULL) {
    (void)fprintf(argv_error_stream,
		  "%s: %s, %d more mandatory argument%s must be specified\n",
		  argv_program, USAGE_ERROR_NAME,
		  mand_c, (mand_c == 1 ? "" : "s"));
  }
  
  if (mand_c > 0 || flag_c > 0) {
    return ERROR;
  }
  else {
    return NOERROR;
  }
}

/*
 * static int check_opt
 *
 * DESCRIPTION:
 *
 * Check for any missing argument options.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * queue_head - Head of the option queue.
 *
 * queue_tail - Tail of the option queue.
 */
static	int	check_opt(const int queue_head, const int queue_tail)
{
  int	queue_c;
  
  queue_c = queue_head - queue_tail;
  if (queue_c > 0) {
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: %s, %d more option-argument%s must be specified\n",
		    argv_program, USAGE_ERROR_NAME,
		    queue_c, (queue_c == 1 ? "" : "s"));
    }
    return ERROR;
  }
  
  return NOERROR;
}

/**************************** argument processing ****************************/

/*
 * static void file_args
 *
 * DESCRIPTION:
 *
 * Read in arguments from a file and process them like they were
 * specified on the command line.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * path -> File of the arguments we are reading in.
 *
 * grid -> Array of argv_t structures we are using.
 *
 * queue_list <-> Our option queue for storing options to arguments.
 *
 * queue_head_p <-> Pointer to integer which will be updated with the
 * head position in our option queue.
 *
 * queue_tail_p <-> Pointer to integer which will be updated with the
 * tail position in our option queue.
 *
 * okay_bp <- Pointer to an integer which is set with 0 if the
 * arguments specified in the env variable are somehow invalid.
 */
static	void	file_args(const char *path, argv_t *grid,
			  argv_t **queue_list, int *queue_head_p,
			  int *queue_tail_p, int *okay_bp)
{
  char	**argv, **argv_p;
  int	arg_c, max;
  FILE	*infile;
  char	line[FILE_LINE_SIZE], *line_p;
  
  /* open the input file */
  if (strcmp(path, "-") == 0) {
    infile = stdin;
  }
  else {
    infile = fopen(path, "r");
  }
  if (infile == NULL) {
    *okay_bp = ARGV_FALSE;
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: could not load command-line arguments from: %s\n",
		    argv_program, path);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return;
  }
  
  /* get an array of char * */
  arg_c = 0;
  max = ARRAY_INCR;
  argv = malloc(sizeof(char *) * max);
  if (argv == NULL) {
    *okay_bp = ARGV_FALSE;
    if (infile != stdin) {
      (void)fclose(infile);
    }
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: memory error during argument processing\n",
		    argv_program);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return;
  }
  argv_p = argv;
  
  /* read in the file lines */
  while (fgets(line, sizeof(line), infile) != NULL) {
    /* punch the \n at end of line */
    for (line_p = line; *line_p != '\n' && *line_p != '\0'; line_p++) {
    }
    *line_p = '\0';
    
    /* skip blank lines */
    if (line_p == line) {
      continue;
    }
    
    *argv_p = string_copy(line);
    if (*argv_p == NULL) {
      *okay_bp = ARGV_FALSE;
      return;
    }
    
    argv_p++;
    arg_c++;
    
    /* do we need to grow the array of pointers? */
    if (arg_c == max) {
      max += ARRAY_INCR;
      argv = realloc(argv, sizeof(char *) * max);
      if (argv == NULL) {
	*okay_bp = ARGV_FALSE;
	if (infile != stdin) {
	  (void)fclose(infile);
	}
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: memory error during argument processing\n",
			argv_program);
	}
	if (argv_interactive) {
	  (void)exit(argv_error_code);
	}
	return;
      }
      argv_p = argv + arg_c;
    }
  }
  
  /* now do the list */
  do_list(grid, arg_c, argv, queue_list, queue_head_p, queue_tail_p, okay_bp);
  
  /* now free up the list */
  for (argv_p = argv; argv_p < argv + arg_c; argv_p++) {
    free(*argv_p);
  }
  free(argv);
  
  if (infile != stdin) {
    (void)fclose(infile);
  }
}

/*
 * static void do_arg
 *
 * DESCRIPTION:
 *
 * Process an argument in MATCH_P which looking at GRID. sets okay_p
 * to FALSE if the argument was not okay.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * grid -> Our array of argv_t structures.
 *
 * match_p -> Entry in our argv_t structure array that matches the
 * specified argument.
 *
 * close_p -> Pointer to the value closely associated (with an '=')
 * with this option or NULL if none.
 *
 * queue_list <-> Our option queue for storing options to arguments.
 *
 * queue_head_p <-> Pointer to integer which will be updated with the
 * head position in our option queue.
 *
 * okay_bp <- Pointer to an integer which is set with 0 if the
 * arguments specified in the env variable are somehow invalid.
 */
static	void	do_arg(argv_t *grid, argv_t *match_p, const char *close_p,
		       argv_t **queue_list, int *queue_head_p, int *okay_bp)
{
  if (! argv_multi_accept_b) {
    /*
     * have we used this one before?
     * NOTE: should this be a warning or a non-error altogether?
     */
    if (match_p->ar_type & ARGV_FLAG_USED
	&& (! (match_p->ar_type & ARGV_FLAG_ARRAY))
	&& ARGV_TYPE(match_p->ar_type) != ARGV_INCR) {
      if (argv_error_stream != NULL) {
	(void)fprintf(argv_error_stream,
		      "%s: %s, you've already specified the '%c' argument\n",
		      argv_program, USAGE_ERROR_NAME,
		      match_p->ar_short_arg);
      }
      *okay_bp = ARGV_FALSE;
    }
  }
  
  /* we used this argument */
  match_p->ar_type |= ARGV_FLAG_USED;
  
  /* check arguments that must be OR'd */
  if (check_or(grid, match_p) != NOERROR) {
    /*
     * don't return here else we might generate an XOR error
     * because the argument wasn't specified
     */
    *okay_bp = ARGV_FALSE;
  }
  
  /*
   * If we have a close argument, pass to translate.  If it is a
   * boolean or increment variable, then pass in a value of null
   * else queue it for needing a value argument.
   */
  if (argv_close_enable_b && close_p != NULL) {
    if (string_to_value(close_p, match_p->ar_variable,
			match_p->ar_type) != NOERROR) {
      *okay_bp = ARGV_FALSE;
    }
  }
  else if (! HAS_ARG(match_p->ar_type)) {
    if (string_to_value(NULL, match_p->ar_variable,
			match_p->ar_type) != NOERROR) {
      *okay_bp = ARGV_FALSE;
    }
  }
  else if (argv_close_enable_b && close_p != NULL) {
    if (string_to_value(close_p, match_p->ar_variable,
			match_p->ar_type) != NOERROR) {
      *okay_bp = ARGV_FALSE;
    }
  }
  else {
    queue_list[*queue_head_p] = match_p;
    (*queue_head_p)++;
  }
}

/*
 * static int is_number
 *
 * DESCRIPTION:
 *
 * Examine an argument string to see if it really is a negative number
 * being passed into a previously specified argument.
 *
 * Thanks much to Nick Kisseberth for pointing out this oversight.
 *
 * RETURNS:
 *
 * 1 if a number otherwise 0.
 *
 * ARGUMENTS:
 *
 * str - String which may be a number.
 */
static	int	is_number(const char *str)
{
  const char	*str_p;
  
  /* empty strings are not numbers */
  if (str[0] == '\0') {
    return 0;
  }
  
  /*
   * All chars in the string should be number chars for it to be a
   * number.  Yes this will return yes if the argument is "00-" but
   * we'll chalk this up to user error.
   */
  for (str_p = str; *str_p != '\0'; str_p++) {
    if (strchr(NUMBER_ARG_CHARS, *str_p) == NULL) {
      return 0;
    }
  }
  
  return 1;
}

/*
 * static void do_list
 *
 * DESCRIPTION:
 *
 * Process a list of arguments with our array of argv_t structures
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * grid - Our array of argv_t structures.
 *
 * arg_c - Number of arguments in argv.
 *
 * argv - User argument array of character pointers.
 *
 * queue_list <-> Our option queue for storing options to arguments.
 *
 * queue_head_p <-> Pointer to integer which will be updated with the
 * head position in our option queue.
 *
 * queue_tail_p <-> Pointer to integer which will be updated with the
 * tail position in our option queue.
 *
 * okay_bp - Pointer to an integer which is set with 0 if the
 * arguments specified in the env variable are somehow invalid.
 */
static	void	do_list(argv_t *grid, const int arg_c, char **argv,
			argv_t **queue_list, int *queue_head_p,
			int *queue_tail_p, int *okay_bp)
{
  argv_t	*grid_p, *match_p;
  int		len, char_c, unwant_c = 0;
  int		last_arg_b = ARGV_FALSE;
  char		*close_p = NULL, **arg_p;
  
  /* run throught rest of arguments */
  for (arg_p = argv; arg_p < argv + arg_c; arg_p++) {
    
    /* have we reached the LAST_ARG marker? */
    if (strcmp(LAST_ARG, *arg_p) == 0) {
      if (last_arg_b) {
	if (argv_last_toggle_b) {
	  last_arg_b = ARGV_FALSE;
	  continue;
	}
      }
      else {
	last_arg_b = ARGV_TRUE;
	continue;
      }
    }
    
    /* are we processing a long option? */
    if ((! last_arg_b)
	&& strncmp(LONG_PREFIX, *arg_p, LONG_PREFIX_LENGTH) == 0) {
      
      /*
       * check for close equals marker
       *
       * NOTE: duplicated in the short prefix section below.  In here otherwise
       * we process normal args with x=5 instead of just -x=5.
       */
      if (argv_close_enable_b) {
	close_p = strchr(*arg_p, ARG_EQUALS);
	/* if we found the special char then punch the null and set pointer */
	if (close_p != NULL) {
	  *close_p = '\0';
	  close_p++;
	}
      }
      
      /* get length of rest of argument */
      len = strlen(*arg_p) - LONG_PREFIX_LENGTH;
      
      /* we need more than the prefix */
      if (len <= 0) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, empty long-option prefix '%s'\n",
			argv_program, USAGE_ERROR_NAME, *arg_p);
	}
	*okay_bp = ARGV_FALSE;
	continue;
      }
      
      match_p = NULL;
      
      /* run though long options looking for a match */
      for (grid_p = grid; grid_p->ar_short_arg != ARGV_LAST; grid_p++) {
	if (grid_p->ar_long_arg == NULL) {
	  continue;
	}
	
	if (strncmp(*arg_p + LONG_PREFIX_LENGTH,
		    grid_p->ar_long_arg, len) == 0) {
	  if (match_p != NULL) {
	    if (argv_error_stream != NULL) {
	      (void)fprintf(argv_error_stream,
			    "%s: %s, '%s' might be '%s' or '%s'\n",
			    argv_program, USAGE_ERROR_NAME, *arg_p,
			    grid_p->ar_long_arg, match_p->ar_long_arg);
	    }
	    *okay_bp = ARGV_FALSE;
	    break;
	  }
	  
	  /* record a possible match */
	  match_p = grid_p;
	  
	  /* don't break, need to see if another one matches */
	}
      }
      
      /* if we found a match but quit then we must have found two matches */
      if (match_p != NULL && grid_p->ar_short_arg != ARGV_LAST) {
	continue;
      }
      
      if (match_p != NULL) {
	(void)do_arg(grid, match_p, close_p, queue_list, queue_head_p,
		     okay_bp);
	continue;
      }
      
      /* we did not find long-option match */
      
      /* check for special file value */
      if (strncmp(FILE_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_close_enable_b && close_p != NULL) {
	  /* open the file and read in the args */
	  file_args(close_p, grid, queue_list, queue_head_p, queue_tail_p,
		    okay_bp);
	}
	else {
	  /* HACK: we enqueue null for the file argument */
	  queue_list[*queue_head_p] = NULL;
	  (*queue_head_p)++;
	}
	continue;
      }
      
      /* check for special usage value */
      if (strncmp(USAGE_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0
	  || strncmp(HELP_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  do_usage(grid, argv_usage_type);
	  (void)exit(0);
	}
	continue;
      }
      
      /* check for special short-usage value */
      if (strncmp(USAGE_SHORT_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  do_usage(grid, ARGV_USAGE_SHORT);
	  (void)exit(0);
	}
	continue;
      }
      
      /* check for special long-usage value */
      if (strncmp(USAGE_LONG_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  do_usage(grid, ARGV_USAGE_LONG);
	  (void)exit(0);
	}
	continue;
      }
      
      /* check for special long-usage value */
      if (strncmp(USAGE_ALL_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  do_usage(grid, ARGV_USAGE_ALL);
	  (void)exit(0);
	}
	continue;
      }
      
      /* check for special help value */
      if (strncmp(HELP_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  if (argv_error_stream != NULL) {
	    if (argv_help_string == NULL) {
	      (void)fprintf(argv_error_stream,
			    "%s: I'm sorry, no help is available.\n",
			    argv_program);
	    }
	    else {
	      (void)fprintf(argv_error_stream, "%s: %s\n",
			    argv_program, argv_help_string);
	    }
	  }
	  (void)exit(0);
	}
	continue;
      }
      
      /* check for special version value */
      if (strncmp(VERSION_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  if (argv_error_stream != NULL) {
	    if (argv_version_string == NULL) {
	      (void)fprintf(argv_error_stream,
			    "%s: no version information is available.\n",
			    argv_program);
	    }
	    else {
	      (void)fprintf(argv_error_stream, "%s: %s%s\n",
			    argv_program, VERSION_LABEL,
			    argv_version_string);
	    }
	  }
	  (void)exit(0);
	}
	continue;
      }
      
      /* check for display arguments value */
      if (strncmp(DISPLAY_ARG, *arg_p + LONG_PREFIX_LENGTH, len) == 0) {
	if (argv_interactive) {
	  if (argv_error_stream != NULL) {
	    display_variables(grid);
	  }
	  (void)exit(0);
	}
	continue;
      }
      
      if (argv_error_stream != NULL) {
	(void)fprintf(argv_error_stream,
		      "%s: %s, unknown long option '%s'.\n",
		      argv_program, USAGE_ERROR_NAME, *arg_p);
      }
      *okay_bp = ARGV_FALSE;
      continue;
    }
    
    /* are we processing a short option? */
    if ((! last_arg_b)
	&& strncmp(SHORT_PREFIX, *arg_p, SHORT_PREFIX_LENGTH) == 0) {
      
      /*
       * check for close equals marker
       *
       * NOTE: duplicated in the long prefix section above.  In here otherwise
       * we process normal args with x=5 instead of just -x=5.
       */
      if (argv_close_enable_b) {
	close_p = strchr(*arg_p, ARG_EQUALS);
	/* if we found the special char then punch the null and set pointer */
	if (close_p != NULL) {
	  *close_p = '\0';
	  close_p++;
	}
      }
      
      /* get length of rest of argument */
      len = strlen(*arg_p) - SHORT_PREFIX_LENGTH;
      
      /* we need more than the prefix */
      if (len <= 0) {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: %s, empty short-option prefix '%s'\n",
			argv_program, USAGE_ERROR_NAME, *arg_p);
	}
	*okay_bp = ARGV_FALSE;
	continue;
      }
      
      /* run through the chars in this option */
      for (char_c = 0; char_c < len; char_c++) {
	
	/* run through the arg list looking for a match */
	for (match_p = grid; match_p->ar_short_arg != ARGV_LAST; match_p++) {
	  if (match_p->ar_short_arg ==
	      (*arg_p)[SHORT_PREFIX_LENGTH + char_c]) {
	    break;
	  }
	}
	
	/* did we not find argument? */
	if (match_p->ar_short_arg == ARGV_LAST) {
	  
	  /* check for special usage value */
	  if ((*arg_p)[SHORT_PREFIX_LENGTH + char_c] == USAGE_CHAR_ARG) {
	    if (argv_interactive) {
	      do_usage(grid, argv_usage_type);
	      (void)exit(0);
	    }
	    continue;
	  }
	  
	  /*
	   * allow values with negative signs if we are at the start
	   * of an argument list, and if the argument is a number, and
	   * we already have a variable looking for a value.  Thanks
	   * to Nick Kisseberth for pointing out this oversight.
	   */
	  if (char_c == 0 && is_number(*arg_p)
	      && *queue_head_p > *queue_tail_p) {
	    
	    match_p = queue_list[*queue_tail_p];
	    /*
	     * NOTE: we don't advance the queue tail here unless we
	     * find out that we can use it below
	     */
	    
	    switch (ARGV_TYPE(match_p->ar_type)) {
	      
	    case ARGV_SHORT:
	    case ARGV_INT:
	    case ARGV_LONG:
	    case ARGV_FLOAT:
	    case ARGV_DOUBLE:
              string_to_value(*arg_p, match_p->ar_variable, match_p->ar_type);
	      char_c = len;
	      /* we actually used it so we advance the queue tail position */
	      (*queue_tail_p)++;
	      continue;
	      break;
	    }
	  }
	  
	  /* create an error string */
	  if (argv_error_stream != NULL) {
	    (void)fprintf(argv_error_stream,
			  "%s: %s, unknown short option '%s%c'.\n",
			  argv_program, USAGE_ERROR_NAME, SHORT_PREFIX,
			  (*arg_p)[SHORT_PREFIX_LENGTH + char_c]);
	  }
	  *okay_bp = ARGV_FALSE;
	  continue;
	}
	
	do_arg(grid, match_p, close_p, queue_list, queue_head_p, okay_bp);
      }
      
      continue;
    }
    
    /* could this be a value? */
    if (grid->ar_short_arg != ARGV_LAST && *queue_head_p > *queue_tail_p) {
      
      /* pull the variable waiting for a value from the queue */
      match_p = queue_list[*queue_tail_p];
      (*queue_tail_p)++;
      
      /* HACK: is this the file argument */
      if (match_p == NULL) {
	file_args(*arg_p, grid, queue_list, queue_head_p, queue_tail_p,
		  okay_bp);
      }
      else {
	if (string_to_value(*arg_p, match_p->ar_variable,
			    match_p->ar_type) != NOERROR) {
	  *okay_bp = ARGV_FALSE;
	}
      }
      continue;
    }
    
    /* process mandatory args if some left to process */
    for (grid_p = grid; grid_p->ar_short_arg != ARGV_LAST; grid_p++) {
      if (grid_p->ar_short_arg == ARGV_MAND
	  && ((! (grid_p->ar_type & ARGV_FLAG_USED))
	      || grid_p->ar_type & ARGV_FLAG_ARRAY)) {
	break;
      }
    }
    if  (grid_p->ar_short_arg != ARGV_LAST) {
      /* absorb another mand. arg */
      if (string_to_value(*arg_p, grid_p->ar_variable,
			  grid_p->ar_type) != NOERROR) {
	*okay_bp = ARGV_FALSE;
      }
      grid_p->ar_type |= ARGV_FLAG_USED;
      continue;
    }
    
    /* process maybe args if some left to process */
    for (grid_p = grid; grid_p->ar_short_arg != ARGV_LAST; grid_p++) {
      if (grid_p->ar_short_arg == ARGV_MAYBE
	  && ((! (grid_p->ar_type & ARGV_FLAG_USED))
	      || grid_p->ar_type & ARGV_FLAG_ARRAY)) {
	break;
      }
    }
    if  (grid_p->ar_short_arg != ARGV_LAST) {
      /* absorb another maybe arg */
      if (string_to_value(*arg_p, grid_p->ar_variable,
			  grid_p->ar_type) != NOERROR) {
	*okay_bp = ARGV_FALSE;
      }
      grid_p->ar_type |= ARGV_FLAG_USED;
      continue;
    }
    
    /* default is an error */
    unwant_c++;
    *okay_bp = ARGV_FALSE;
  }
  
  if (unwant_c > 0 && argv_error_stream != NULL) {
    (void)fprintf(argv_error_stream,
		  "%s: %s, %d unwanted additional argument%s\n",
		  argv_program, USAGE_ERROR_NAME,
		  unwant_c, (unwant_c == 1 ? "" : "s"));
  }
}

/****************************** env processing *******************************/

/*
 * static int do_env_args
 *
 * DESCRIPTION:
 *
 * Handle the args from the environmentatl variable.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * args - Array of argv_t structures we are using.
 *
 * queue_list <-> Our option queue for storing options to arguments.
 *
 * queue_head_p <-> Pointer to integer which will be updated with the
 * head position in our option queue.
 *
 * queue_tail_p <-> Pointer to integer which will be updated with the
 * tail position in our option queue.
 *
 * okay_bp - Pointer to an integer which is set with 0 if the
 * arguments specified in the env variable are somehow invalid.
 */
static	int	do_env_args(argv_t *args, argv_t **queue_list,
			    int *queue_head_p, int *queue_tail_p, int *okay_bp)
{
  int	env_c, env_n;
  char	**vect_p, env_name[1024], *environ_p;
  
  /* create the env variable */
  (void)loc_snprintf(env_name, sizeof(env_name), ENVIRON_FORMAT, argv_program);
  
  /* NOTE: by default the env name is all uppercase */
  for (environ_p = env_name; *environ_p != '\0'; environ_p++) {
    if (islower(*environ_p)) {
      *environ_p = toupper(*environ_p);
    }
  }
  
  environ_p = getenv(env_name);
  if (environ_p == NULL) {
    return NOERROR;
  }
  
  /* break the list into tokens and do the list */
  environ_p = string_copy(environ_p);
  if (environ_p == NULL) {
    return ERROR;
  }
  
  vect_p = vectorize(environ_p, " \t", &env_n);
  if (vect_p != NULL) {
    do_list(args, env_n, vect_p, queue_list, queue_head_p, queue_tail_p,
	    okay_bp);
    
    /* free token list */
    for (env_c = 0; env_c < env_n; env_c++) {
      free(vect_p[env_c]);
    }
    free(vect_p);
  }
  free(environ_p);
  
  return NOERROR;
}

/*
 * static int process_env
 *
 * DESCRIPTION:
 *
 * Process the global env variables.
 *
 * RETURNS:
 *
 * Success - 0
 *
 * Faulure - -1
 *
 * ARGUMENTS:
 *
 * None.
 */
static	int	process_env(void)
{
  static int	done_b = ARGV_FALSE;
  char		*env_val, *tok_p, *env_p;
  int		len;
  
  /* make sure we only do this once */
  if (done_b) {
    return NOERROR;
  }
  
  done_b = ARGV_TRUE;
  
  /* get the argv information */
  env_val = getenv(GLOBAL_NAME);
  if (env_val == NULL) {
    return NOERROR;
  }
  
  /* save a copy of it */
  env_val = string_copy(env_val);
  if (env_val == NULL) {
    return ERROR;
  }
  
  env_p = env_val;
  
  for (;;) {
    tok_p = strsep(&env_p, " \t,:");
    if (tok_p == NULL) {
      break;
    }
    /* skip any empty tokens */
    if (*tok_p == '\0') {
      continue;
    }
    
    len = strlen(GLOBAL_CLOSE);
    if (strncmp(GLOBAL_CLOSE, tok_p, len) == 0) {
      tok_p += len;
      if (strcmp(tok_p, "disable") == 0
	  || strcmp(tok_p, "off") == 0
	  || strcmp(tok_p, "no") == 0
	  || strcmp(tok_p, "0") == 0) {
	argv_close_enable_b = 0;
      }
      else if (strcmp(tok_p, "enable") == 0
	       || strcmp(tok_p, "on") == 0
	       || strcmp(tok_p, "yes") == 0
	       || strcmp(tok_p, "1") == 0) {
	argv_close_enable_b = 1;
      }
      else {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: illegal env variable '%s' '%s' argument '%s'\n",
			__FILE__, GLOBAL_NAME, GLOBAL_CLOSE, tok_p);
	}
      }
      continue;
    }
    
    len = strlen(GLOBAL_LASTTOG);
    if (strncmp(GLOBAL_LASTTOG, tok_p, len) == 0) {
      tok_p += len;
      if (strcmp(tok_p, "disable") == 0
	  || strcmp(tok_p, "off") == 0
	  || strcmp(tok_p, "no") == 0
	  || strcmp(tok_p, "0") == 0) {
	argv_last_toggle_b = 0;
      }
      else if (strcmp(tok_p, "enable") == 0
	       || strcmp(tok_p, "on") == 0
	       || strcmp(tok_p, "yes") == 0
	       || strcmp(tok_p, "1") == 0) {
	argv_last_toggle_b = 1;
      }
      else {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: illegal env variable '%s' '%s' argument '%s'\n",
			__FILE__, GLOBAL_NAME, GLOBAL_LASTTOG, tok_p);
	}
      }
      continue;
    }
    
    len = strlen(GLOBAL_ENV);
    if (strncmp(GLOBAL_ENV, tok_p, len) == 0) {
      tok_p += len;
      if (strcmp(tok_p, "none") == 0) {
	argv_process_env_b = 0;
	argv_env_after_b = 0;
      }
      else if (strcmp(tok_p, "before") == 0) {
	argv_process_env_b = 1;
	argv_env_after_b = 0;
      }
      else if (strcmp(tok_p, "after") == 0) {
	argv_process_env_b = 1;
	argv_env_after_b = 1;
      }
      else {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: illegal env variable '%s' '%s' argument '%s'\n",
			__FILE__, GLOBAL_NAME, GLOBAL_ENV, tok_p);
	}
      }
      continue;
    }
    
    len = strlen(GLOBAL_ERROR);
    if (strncmp(GLOBAL_ERROR, tok_p, len) == 0) {
      tok_p += len;
      if (strcmp(tok_p, "none") == 0) {
	argv_error_type = ARGV_USAGE_NONE;
      }
      else if (strcmp(tok_p, "see") == 0) {
	argv_error_type = ARGV_USAGE_SEE;
      }
      else if (strcmp(tok_p, "short") == 0) {
	argv_error_type = ARGV_USAGE_SHORT;
      }
      else if (strcmp(tok_p, "shortrem") == 0) {
	argv_error_type = ARGV_USAGE_SHORT_REM;
      }
      else if (strcmp(tok_p, "long") == 0) {
	argv_error_type = ARGV_USAGE_LONG;
      }
      else if (strcmp(tok_p, "all") == 0) {
	argv_error_type = ARGV_USAGE_ALL;
      }
      else {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: illegal env variable '%s' '%s' argument '%s'\n",
			__FILE__, GLOBAL_NAME, GLOBAL_ERROR, tok_p);
	}
      }
      continue;
    }
    
    len = strlen(GLOBAL_MULTI);
    if (strncmp(GLOBAL_MULTI, tok_p, len) == 0) {
      tok_p += len;
      if (strcmp(tok_p, "reject") == 0) {
	argv_multi_accept_b = 0;
      }
      else if (strcmp(tok_p, "accept") == 0) {
	argv_multi_accept_b = 1;
      }
      else {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: illegal env variable '%s' '%s' argument '%s'\n",
			__FILE__, GLOBAL_NAME, GLOBAL_MULTI, tok_p);
	}
      }
      continue;
    }
    
    len = strlen(GLOBAL_USAGE);
    if (strncmp(GLOBAL_USAGE, tok_p, len) == 0) {
      tok_p += len;
      if (strcmp(tok_p, "short") == 0) {
	argv_usage_type = ARGV_USAGE_SHORT;
      }
      else if (strcmp(tok_p, "shortrem") == 0) {
	argv_usage_type = ARGV_USAGE_SHORT_REM;
      }
      else if (strcmp(tok_p, "long") == 0) {
	argv_usage_type = ARGV_USAGE_LONG;
      }
      else if (strcmp(tok_p, "all") == 0) {
	argv_usage_type = ARGV_USAGE_ALL;
      }
      else {
	if (argv_error_stream != NULL) {
	  (void)fprintf(argv_error_stream,
			"%s: illegal env variable '%s' '%s' argument '%s'\n",
			__FILE__, GLOBAL_NAME, GLOBAL_USAGE, tok_p);
	}
      }
      continue;
    }
    
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: illegal env variable '%s' setting '%s'\n",
		    __FILE__, GLOBAL_NAME, tok_p);
    }
  }
  
  free(env_val);
  return NOERROR;
}

/***************************** exported routines *****************************/

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
int	argv_process_no_env(argv_t *args, const int arg_n, char **argv)
{
  int		entry_c;
  const char	*prog_p;
  int		okay_b = ARGV_TRUE;
  argv_t	*arg_p;
  argv_t	**queue_list = NULL;
  int		queue_head = 0, queue_tail = 0;
  
  if (args == NULL) {
    args = empty;
  }
  
  if (arg_n < 0) {
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: %s, argc argument to argv_process is %d\n",
		    __FILE__, INTERNAL_ERROR_NAME, arg_n);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return ERROR;
  }
  
  if (argv == NULL) {
    if (argv_error_stream != NULL) {
      (void)fprintf(argv_error_stream,
		    "%s: %s, argv argument to argv_process is NULL\n",
		    __FILE__, INTERNAL_ERROR_NAME);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return ERROR;
  }
  
  /* set global variables */
  argv_argv = argv;
  argv_argc = arg_n;
  
  /* build the program name from the argv[0] path */
  {
    const char	*tmp_p;
    
    prog_p = *argv;
    for (tmp_p = *argv; *tmp_p != '\0'; tmp_p++) {
      if (*tmp_p == '/') {
	prog_p = tmp_p + 1;
      }
    }
  }
  
  /* so we can step on the environmental space */
  (void)strncpy(argv_program, prog_p, PROGRAM_NAME);
  
  /* count the args */
  entry_c = 0;
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    entry_c++;
  }
  
  /* verify the argument array */
  if (preprocess_array(args, entry_c) != NOERROR) {
    return ERROR;
  }
  
  /* allocate our value queue */
  if (arg_n > 0) {
    /* allocate our argument queue */
    queue_list = (argv_t **)malloc(sizeof(argv_t *) * arg_n);
    if (queue_list == NULL) {
      return ERROR;
    }
    queue_head = 0;
    queue_tail = 0;
  }
  
  /* do the env args before? */
  if (argv_process_env_b && (! argv_env_after_b)) {
    if (do_env_args(args, queue_list, &queue_head, &queue_tail,
		    &okay_b) != NOERROR) {
      return ERROR;
    }
  }
  
  /* do the external args */
  do_list(args, arg_n - 1, argv + 1, queue_list, &queue_head, &queue_tail,
	  &okay_b);
  
  /* DO the env args after? */
  if (argv_process_env_b && argv_env_after_b) {
    if (do_env_args(args, queue_list, &queue_head, &queue_tail,
		    &okay_b) != NOERROR) {
      return ERROR;
    }
  }
  
  /* make sure the XOR and MAND args and argument-options are okay */
  if (check_mand(args) != NOERROR) {
    okay_b = ARGV_FALSE;
  }
  if (check_opt(queue_head, queue_tail) != NOERROR) {
    okay_b = ARGV_FALSE;
  }
  if (check_xor(args) != NOERROR) {
    okay_b = ARGV_FALSE;
  }
  
  /* if we allocated the space then free it */
  if (arg_n > 0) {
    free(queue_list);
  }
  
  /* was there an error? */
  if (! okay_b) {
    if (argv_error_stream != NULL) {
      do_usage(args, argv_error_type);
    }
    if (argv_interactive) {
      (void)exit(argv_error_code);
    }
    return ERROR;
  }
  
  return NOERROR;
}

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
int	argv_process(argv_t *args, const int argc, char **argv)
{
  if (! enabled_b) {
    argv_startup();
  }
  
  /* we only process env variables here */
  if (process_env() != NOERROR) {
    return ERROR;
  }
  
  if (argv_process_no_env(args, argc, argv) == NOERROR) {
    return NOERROR;
  }
  else {
    return ERROR;
  }
}

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
int	argv_usage(const argv_t *args, const int which)
{
  if (! enabled_b) {
    argv_startup();
  }
  
  if (process_env() != NOERROR) {
    return ERROR;
  }
  
  if (args == NULL) {
    args = empty;
  }
  
  if (which == ARGV_USAGE_SHORT
      || which == ARGV_USAGE_LONG
      || which == ARGV_USAGE_ALL) {
    do_usage(args, which);
  }
  else {
    /* default/env settings */
    do_usage(args, argv_usage_type);
  }
  
  return NOERROR;
}

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
int	argv_was_used(const argv_t *args, const char short_arg)
{
  const argv_t	*arg_p;
  
  if (! enabled_b) {
    argv_startup();
  }
  
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    if (arg_p->ar_short_arg == short_arg) {
      if (arg_p->ar_type & ARGV_FLAG_USED) {
	return 1;
      }
      else {
	return 0;
      }
    }
  }
  
  return 0;
}

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
int	argv_long_was_used(const argv_t *args, const char *long_arg)
{
  const argv_t	*arg_p;
  
  if (! enabled_b) {
    argv_startup();
  }
  
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    if (arg_p->ar_long_arg == long_arg) {
      if (arg_p->ar_type & ARGV_FLAG_USED) {
	return 1;
      }
      else {
	return 0;
      }
    }
  }
  
  return 0;
}

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
int	argv_entry_was_used(const argv_t *argv_entry_p)
{
  if (argv_entry_p->ar_type & ARGV_FLAG_USED) {
    return 1;
  }
  else {
    return 0;
  }
}

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
void	argv_cleanup(const argv_t *args)
{
  const argv_t	*arg_p;
  int		entry_c;
  
  if (! enabled_b) {
    argv_startup();
  }
  
  if (args == NULL) {
    return;
  }
  
  /* run through the argument structure */
  for (arg_p = args; arg_p->ar_short_arg != ARGV_LAST; arg_p++) {
    /* handle any arrays */
    if (arg_p->ar_type & ARGV_FLAG_ARRAY) {
      argv_array_t	*arr_p = (argv_array_t *)arg_p->ar_variable;
      
      /* free any entries */
      if (arr_p->aa_entry_n > 0) {
	if (ARGV_TYPE(arg_p->ar_type) == ARGV_CHAR_P) {
	  for (entry_c = 0; entry_c < arr_p->aa_entry_n; entry_c++) {
	    free(ARGV_ARRAY_ENTRY(*arr_p, char *, entry_c));
	  }
	}
	free(arr_p->aa_entries);
      }
      arr_p->aa_entries = NULL;
      arr_p->aa_entry_n = 0;
      continue;
    }
    
    /* handle individual charps */
    if (arg_p->ar_type & ARGV_FLAG_USED
	&& ARGV_TYPE(arg_p->ar_type) == ARGV_CHAR_P) {
      free(*(char **)arg_p->ar_variable);
      continue;
    }
  }
}

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
int	argv_copy_args(char *buf, const int buf_size)
{
  char	**argv_p, *buf_p = buf, *arg_p;
  int	arg_c, size_c = buf_size;
  
  if (! enabled_b) {
    argv_startup();
  }
  
  if (buf_size <= 0) {
    return NOERROR;
  }
  
  *buf_p = '\0';
  
  if (argv_argv == NULL || buf_size == 1) {
    return NOERROR;
  }
  
  for (argv_p = argv_argv + 1, arg_c = 1;
       arg_c < argv_argc;
       argv_p++, arg_c++) {
    
    /* we compare against 2 for the ' ' and the \0 */
    if (size_c < 2) {
      break;
    }
    
    if (argv_p > argv_argv + 1) {
      *buf_p++ = ' ';
      size_c--;
    }
    
    /* we always compare against 2 to include the \0 */
    for (arg_p = *argv_p; *arg_p != '\0' && size_c >= 2; size_c--) {
      *buf_p++ = *arg_p++;
    }
  }
  
  *buf_p = '\0';
  return NOERROR;
}

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
int	argv_value_string(const argv_t *argv_entry_p, char *buf,
			  const int buf_size)
{
  argv_array_t	*arr_p;
  int		ret, len;
  char		details[128];
  
  if (! enabled_b) {
    argv_startup();
  }
  
  /* do we have an array here? */
  if (argv_entry_p->ar_type & ARGV_FLAG_ARRAY) {
    
    /* if we have an array, then  */
    arr_p = (argv_array_t *)argv_entry_p->ar_variable;
    if (arr_p->aa_entry_n == 0) {
      strncpy(buf, "0 array entries", buf_size);
      buf[buf_size - 1] = '\0';
      ret = strlen(buf);
    }
    else {
      len = value_to_string(arr_p->aa_entries,
			    ARGV_TYPE(argv_entry_p->ar_type),
			    buf, buf_size);
      if (arr_p->aa_entry_n == 1) {
	ret = len;
      }
      else {
	(void)loc_snprintf(details, sizeof(details), " (1st of %d entries)",
			   arr_p->aa_entry_n);
	strncpy(buf + len, details, buf_size - len);
	buf[buf_size - 1] = '\0';
	ret = strlen(buf);
      }
    }
  }
  else {
    ret = value_to_string(argv_entry_p->ar_variable, argv_entry_p->ar_type,
			  buf, buf_size);
  }
  
  return ret;
}

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
const char	*argv_type_info(const unsigned int type, unsigned int *size_p,
				const char **desc_p)
{
  unsigned int	val_type;
  argv_type_t	*type_p;
  
  val_type = ARGV_TYPE(type);
  
  for (type_p = argv_types; type_p->at_value != 0; type_p++) {
    if (type_p->at_value == val_type) {
      if (size_p != NULL) {
	*size_p = type_p->at_size;
      }
      if (desc_p != NULL) {
	*desc_p = type_p->at_desc;
      }
      return type_p->at_name;
    }
  }
  
  if (size_p != NULL) {
    *size_p = 0;
  }
  if (desc_p != NULL) {
    *desc_p = "Unknown type";
  }
  return "(unknown type)";
}
