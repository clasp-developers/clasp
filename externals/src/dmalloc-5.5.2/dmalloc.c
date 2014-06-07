/*
 * program that handles the dmalloc variables.
 *
 * Copyright 2000 by Gray Watson
 *
 * This file is part of the dmalloc package.
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
 * $Id: dmalloc.c,v 1.120 2007/05/14 17:09:20 gray Exp $
 */

/*
 * This is the dmalloc program which is designed to enable the user to
 * easily set the environmental variables that control the dmalloc
 * library capabilities.
 */
/*
 * NOTE: all standard-output from this program is designed to be run
 * through a shell evaluation command by default.  Any messages for
 * the user should therefore be send to stderr.
 */

#include <stdio.h>				/* for stderr */

#define DMALLOC_DISABLE

#if HAVE_STRING_H
# include <string.h>
#endif
#if HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "conf.h"
#include "dmalloc_argv.h"			/* for argument processing */
#include "dmalloc.h"

#include "compat.h"
#include "debug_tok.h"
#include "debug_tok.h"
#include "env.h"
#include "error_val.h"
#include "dmalloc_loc.h"
#include "version.h"

#define HOME_ENVIRON	"HOME"			/* home directory */
#define SHELL_ENVIRON	"SHELL"			/* for the type of shell */
#define DEFAULT_CONFIG	".dmallocrc"		/* default config file */
#define TOKENIZE_EQUALS	" \t="			/* for tag lines */
#define TOKENIZE_CHARS	" \t,"			/* for tag lines */

#define DEBUG_ARG		'd'		/* debug argument */
#define INTERVAL_ARG		'i'		/* interval argument */
#define THREAD_LOCK_ON_ARG	'o'		/* lock-on argument */
#define LIMIT_ARG		'M'		/* memory-limit argument */
#define LINE_WIDTH		75		/* num debug toks per line */

#define FILE_NOT_FOUND		1
#define FILE_FOUND		2
#define TOKEN_FOUND		3

/*
 * default flag information
 */
typedef struct {
  char		*de_string;			/* default name */
  long		de_flags;			/* default settings */
} default_t;

#define RUNTIME_FLAGS	(DEBUG_LOG_STATS | DEBUG_LOG_NONFREE | \
			 DEBUG_LOG_BAD_SPACE | \
			 DEBUG_CHECK_FENCE | \
			 DEBUG_CATCH_NULL)
#define LOW_FLAGS	(RUNTIME_FLAGS | \
			 DEBUG_LOG_ELAPSED_TIME | \
			 DEBUG_CHECK_SHUTDOWN | \
			 DEBUG_FREE_BLANK | DEBUG_ERROR_ABORT | \
			 DEBUG_ALLOC_BLANK)
#define MEDIUM_FLAGS	(LOW_FLAGS | \
			 DEBUG_CHECK_HEAP | \
			 DEBUG_REALLOC_COPY)
#define HIGH_FLAGS	(MEDIUM_FLAGS | \
			 DEBUG_CHECK_BLANK | DEBUG_CHECK_FUNCS)
#define ALL_FLAGS	(HIGH_FLAGS | \
			 DEBUG_LOG_TRANS | DEBUG_LOG_ADMIN | \
			 DEBUG_NEVER_REUSE)
/* NOTE: print-messages is not in this list because it is special */

static	default_t	defaults[] = {
  { "none",		0 },
  { "runtime",		RUNTIME_FLAGS },
  { "run",		RUNTIME_FLAGS },
  { "low",		LOW_FLAGS },
  { "med",		MEDIUM_FLAGS },
  { "medium",		MEDIUM_FLAGS },
  { "high",		HIGH_FLAGS },
  { "all",		ALL_FLAGS },
  { NULL }
};

/* argument variables */
static	int	bourne_b = 0;			/* set bourne shell output */
static	int	cshell_b = 0;			/* set c-shell output */
static	int	gdb_b = 0;			/* set gdb output */
static	int	rcshell_b = 0;			/* set rc shell output */

static	char	*address = NULL;		/* for ADDRESS */
static	int	clear_b = 0;			/* clear variables */
static	int	debug = 0;			/* for DEBUG */
static	int	errno_to_print = 0;		/* to print the error string */
static	int	help_b = 0;			/* print help message */
static	char	*inpath = NULL;			/* for config-file path */
static	unsigned long interval = 0;		/* for setting INTERVAL */
static	int	thread_lock_on = 0;		/* for setting LOCK_ON */
static	int	keep_b = 0;			/* keep settings override -r */
static	int	list_tags_b = 0;		/* list rc tags */
static	int	debug_tokens_b = 0;		/* list debug tokens */
static	char	*logpath = NULL;		/* for LOGFILE setting */
static	int	long_tokens_b = 0;		/* long-tok output */
static	argv_array_t	minus;			/* tokens to remove */
static	unsigned long limit_arg = 0;		/* memory limit */
static	int	make_changes_b = 1;		/* make no changes to env */
static	argv_array_t	plus;			/* tokens to add */
static	int	remove_auto_b = 0;		/* auto-remove settings */
static	char	*start_file = NULL;		/* for START settings */
static	unsigned long start_iter = 0;		/* for START settings */
static	unsigned long start_size = 0;		/* for START settings */
static	int	usage_b = 0;			/* usage messages */
static	int	verbose_b = 0;			/* verbose flag */
static	int	very_verbose_b = 0;		/* very-verbose flag */
static	int	version_b = 0;			/* print version string */
static	char	*tag = NULL;			/* maybe a tag argument */

static	argv_t	args[] = {
  { 'b',	"bourne-shell",	ARGV_BOOL_INT,	&bourne_b,
    NULL,			"set output for bourne shells" },
  { ARGV_OR },
  { 'C',	"c-shell",	ARGV_BOOL_INT,	&cshell_b,
    NULL,			"set output for C-type shells" },
  { ARGV_OR },
  { 'g',	"gdb",		ARGV_BOOL_INT,	&gdb_b,
    NULL,			"set output for gdb parsing" },
  { ARGV_OR },
  { 'R',	"rc-shell",	ARGV_BOOL_INT,	&rcshell_b,
    NULL,			"set output for rc shell" },
  
  { 'a',	"address",	ARGV_CHAR_P,	&address,
    "address:#",		"stop when malloc sees address" },
  { 'c',	"clear",	ARGV_BOOL_INT,	&clear_b,
    NULL,			"clear all variables not set" },
  { DEBUG_ARG,	"debug-mask",	ARGV_HEX,	&debug,
    "value",			"hex flag to set debug mask" },
  { 'D',	"debug-tokens",	ARGV_BOOL_INT,	&debug_tokens_b,
    NULL,			"list debug tokens" },
  { 'e',	"errno",	ARGV_INT,	&errno_to_print,
    "errno",			"print error string for errno" },
  { 'f',	"file",		ARGV_CHAR_P,	&inpath,
    "path",			"config if not $HOME/.dmallocrc" },
  { 'h',	"help",		ARGV_BOOL_INT,	&help_b,
    NULL,			"print help message" },
  { INTERVAL_ARG, "interval",	ARGV_U_LONG,	&interval,
    "value",			"check heap every number times" },
  { 'k',	"keep",		ARGV_BOOL_INT,	&keep_b,
    NULL,			"keep settings (override -r)" },
  { 'l',	"logfile",	ARGV_CHAR_P,	&logpath,
    "path",			"file to log messages to" },
  { 'L',	"long-tokens",	ARGV_BOOL_INT,	&long_tokens_b,
    NULL,			"set env with tokens not 0x..." },
  { 'm',	"minus",	ARGV_CHAR_P | ARGV_FLAG_ARRAY,	&minus,
    "token(s)",			"del tokens from current debug" },
  { LIMIT_ARG,	"memory-limit",	ARGV_U_SIZE,	&limit_arg,
    "value",			"limit allocations to this amount" },
  { 'n',	"no-changes",	ARGV_BOOL_NEG,	&make_changes_b,
    NULL,			"make no changes to the env" },
  { THREAD_LOCK_ON_ARG, "lock-on", ARGV_INT,	&thread_lock_on,
    "number",			"number of times to not lock" },
  { 'p',	"plus",		ARGV_CHAR_P | ARGV_FLAG_ARRAY,	&plus,
    "token(s)",			"add tokens to current debug" },
  { 'r',	"remove",	ARGV_BOOL_INT,	&remove_auto_b,
    NULL,			"remove other settings if tag" },
  
  { 's',	"start-file",	ARGV_CHAR_P,	&start_file,
    "file:line",		"check heap after this location" },
  { ARGV_OR },
  { 'S',	"start-iter",	ARGV_U_LONG,	&start_iter,
    "number",			"check heap after this iteration" },
  { ARGV_OR },
  { '\0',	"start-size",	ARGV_U_SIZE,	&start_size,
    "size",			"check heap after this mem size" },
  
  { 't',	"list-tags",	ARGV_BOOL_INT,	&list_tags_b,
    NULL,			"list tags in rc file" },
  { 'u',	"usage",	ARGV_BOOL_INT,	&usage_b,
    NULL,			"print usage messages" },
  { 'v',	"verbose",	ARGV_BOOL_INT,	&verbose_b,
    NULL,			"turn on verbose output" },
  { 'V',	"very-verbose",	ARGV_BOOL_INT,	&very_verbose_b,
    NULL,			"turn on very-verbose output" },
  { '\0',	"version",	ARGV_BOOL_INT,	&version_b,
    NULL,			"display version string" },
  { ARGV_MAYBE,	NULL,		ARGV_CHAR_P,	&tag,
    "tag",			"debug token, internal or from rc file" },
  { ARGV_LAST }
};

/*
 * list of bourne shells
 */
static	char	*sh_shells[] = { "sh", "ash", "bash", "ksh", "zsh", NULL };

/*
 * try a check out the shell env variable to see what form of shell
 * commands we should output
 */
static	void	choose_shell(void)
{
  const char	*shell, *shell_p;
  int		shell_c;
  
  shell = getenv(SHELL_ENVIRON);
  if (shell == NULL) {
    /* oh well, we just guess on c-shell */
    cshell_b = 1;
    return;
  }
  
  shell_p = strrchr(shell, '/');
  if (shell_p == NULL) {
    shell_p = shell;
  }
  else {
    shell_p++;
  }
  
  /* look for the rc shell specifically */
  if (strcmp("rc", shell_p) == 0) {
    rcshell_b = 1;
    return;
  }
  
  /* look for the shells which are bourne-shell compatible */
  for (shell_c = 0; sh_shells[shell_c] != NULL; shell_c++) {
    if (strcmp(sh_shells[shell_c], shell_p) == 0) {
      bourne_b = 1;
      return;
    }
  }
  
  /* otherwise set to c-shell */
  cshell_b = 1;
}

/*
 * dump the current flags set in the debug variable VAL
 */
static	void	dump_debug(const unsigned long val)
{
  attr_t	*attr_p;
  char		*str;
  unsigned long	work = val;
  int		col_c = 0, len;
  
  for (attr_p = attributes; attr_p->at_string != NULL; attr_p++) {
    /* the below test for work == 0 is necessary to handle the 'none' token */
    if ((work == 0 && attr_p->at_value == 0)
	|| (attr_p->at_value != 0 && BIT_IS_SET(work, attr_p->at_value))) {
      BIT_CLEAR(work, attr_p->at_value);
      
      if (col_c == 0) {
	(void)fprintf(stderr, "   ");
	col_c += 3;
      }
      
      if (very_verbose_b) {
	(void)fprintf(stderr, "%s -- %s (%#lx)\n",
		      attr_p->at_string, attr_p->at_desc, attr_p->at_value);
	col_c = 0;
      }
      else {
	str = attr_p->at_string;
	len = strlen(str);
	if (col_c + len + 2 > LINE_WIDTH) {
	  (void)fprintf(stderr, "\n");
	  (void)fprintf(stderr, "   ");
	  col_c = 3;
	}
	(void)fprintf(stderr, "%s", str);
	col_c += len;
	
	/* if we've got more to go then print the , */
	if (work != 0) {
	  (void)fprintf(stderr, ", ");
	  col_c += 2;
	}
      }
      
      if (work == 0) {
	break;
      }
    }
  }
  
  if (col_c != 0) {
    (void)fprintf(stderr, "\n");
  }
  
  if (work != 0) {
    (void)fprintf(stderr, "%s: warning, unknown debug flag(s): %#lx\n",
		  argv_program, work);
  }
}

/*
 * translate TOK into its proper value which is returned
 */
static	long	token_to_value(const char *tok)
{
  attr_t	*attr_p;
  
  /* find the matching attribute string */
  for (attr_p = attributes; attr_p->at_string != NULL; attr_p++) {
    if (strcmp(tok, attr_p->at_string) == 0) {
      break;
    }
  }
  
  if (attr_p->at_string == NULL) {
    (void)fprintf(stderr, "%s: unknown token '%s'\n", argv_program, tok);
    return 0;
  }
  
  /* if we have a 0 value and not none then this is a disabled token */
  if (attr_p->at_value == 0 && strcmp(tok, "none") != 0) {
    (void)fprintf(stderr, "%s: token '%s' has been disabled: %s\n",
		  argv_program, tok, attr_p->at_desc);
    return 0;
  }
  
  return attr_p->at_value;
}

/*
 * Read in the next token from INFILE.  It passes back the returned
 * debug value in DEBUG_P.  Passes back the matching TOKEN of
 * TOKEN_SIZE.  Returns 1 if there was a next else 0.
 */
static	int	read_next_token(FILE *infile, long *debug_p,
				char *token, const int token_size)
{
  int	cont_b = 0, found_b = 0;
  long	new_debug = 0;
  char	buf[1024], *tok_p, *buf_p;
  
  while (fgets(buf, sizeof(buf), infile) != NULL) {
    
    /* ignore comments and empty lines */
    if (buf[0] == '#' || buf[0] == '\n') {
      continue;
    }
    
    /* chop off the ending \n */
    buf_p = strrchr(buf, '\n');
    SET_POINTER(buf_p, '\0');
    
    buf_p = buf;
    
    /* if we're not continuing then we need to process a tag */
    if (! cont_b) {
      
      /* get the first token on the line */
      tok_p = strsep(&buf_p, TOKENIZE_EQUALS);
      if (tok_p == NULL) {
	continue;
      }
      if (*tok_p == '\0') {
	(void)fprintf(stderr, "Invalid start of line: %s\n", buf_p);
	continue;
      }
      
      /* save the token */
      if (token != NULL) {
	(void)strncpy(token, tok_p, token_size);
	token[token_size - 1] = '\0';
      }
      found_b = 1;
    }
    
    cont_b = 0;
    
    while (1) {
      /* get the next token */
      tok_p = strsep(&buf_p, TOKENIZE_CHARS);
      if (tok_p == NULL) {
	break;
      }
      if (*tok_p == '\0') {
	continue;
      }
      
      /* do we have a continuation character? */
      if (strcmp(tok_p, "\\") == 0) {
	cont_b = 1;
	break;
      }
      
      new_debug |= token_to_value(tok_p);
    }
    
    /* are we done? */
    if (! cont_b) {
      break;
    }
  }
  
  SET_POINTER(debug_p, new_debug);
  
  if (found_b) {
    return 1;
  }
  else {
    return 0;
  }
}

/*
 * Read in a rc file from PATH and process it looking for the
 * specified DEBUG_VALUE or TAG_FIND token.  It passes back the
 * returned debug value in DEBUG_P.  Passes back the matching TOKEN of
 * TOKEN_SIZE.
 *
 * Returns FILE_NOT_FOUND, FILE_FOUND, or TOKEN_FOUND.
 */
static	int	read_rc_file(const char *path, const long debug_value,
			     const char *tag_find, long *debug_p,
			     char *token, const int token_size)
{
  FILE	*infile;
  int	found_b = 0;
  char	next_token[64];
  long	new_debug;
  
  /* open the path */
  infile = fopen(path, "r");
  if (infile == NULL) {
    return FILE_NOT_FOUND;
  }
  
  /* run through the tokens, looking for a match */
  while (read_next_token(infile, &new_debug, next_token,
			 sizeof(next_token)) == 1) {
    
    /* are we looking for a tag? */
    if (tag_find != NULL && strcmp(tag_find, next_token) == 0) {
      found_b = 1;
      break;
    }
    
    /* are we looking for a debug-value? */
    if (debug_value > 0 && debug_value == new_debug) {
      found_b = 1;
      break;
    }
  }
  
  (void)fclose(infile);
  
  SET_POINTER(debug_p, new_debug);
  if (token != NULL) {
    (void)loc_snprintf(token, token_size, "config file token: %s",
		       next_token);
  }
  
  if (found_b) {
    return TOKEN_FOUND;
  }
  else {
    return FILE_FOUND;
  }
}

/*
 * Process the user configuration looking for the TAG_FIND.  If it is
 * null then look for DEBUG_VALUE in the file and copy the token found
 * into TOKEN of TOKEN_SIZE.  Routine returns the new debug value
 * matching tag.
 */
static	long	find_tag(const long debug_value, const char *tag_find,
			 char *token, const int token_size)
{
  char		path[1024], *path_p;
  default_t	*def_p;
  const char	*home_p;
  int		ret;
  long		new_debug = 0;
  
  /* do we need to have a home variable? */
  if (inpath == NULL) {
    
    /* first we try to read the RC file from the current directory */
    ret = read_rc_file(DEFAULT_CONFIG, debug_value, tag_find, &new_debug,
		       token, token_size);
    /* did we find the correct value in the file? */
    if (ret == TOKEN_FOUND) {
      return new_debug;
    }
    
    /* if we did not find the file, check the home directory */
    if (ret == FILE_FOUND) {
      path_p = DEFAULT_CONFIG;
    }
    else {
      /* find our home directory */
      home_p = getenv(HOME_ENVIRON);
      if (home_p == NULL) {
	(void)fprintf(stderr, "%s: could not find variable '%s'\n",
		      argv_program, HOME_ENVIRON);
	exit(1);
      }
      
      (void)loc_snprintf(path, sizeof(path), "%s/%s", home_p, DEFAULT_CONFIG);
      
      /* read in the file from our home directory */
      ret = read_rc_file(path, debug_value, tag_find, &new_debug,
			 token, token_size);
      /* did we find the correct value in the file? */
      if (ret == TOKEN_FOUND) {
	return new_debug;
      }
      if (ret == FILE_FOUND) {
	path_p = path;
      }
      else {
	path_p = NULL;
      }
    }
  }
  else {
    /* read in the specified file */
    ret = read_rc_file(inpath, debug_value, tag_find, &new_debug,
		       token, token_size);
    /* did we find the correct value in the file? */
    if (ret == TOKEN_FOUND) {
      return new_debug;
    }
    /* if the specified was not found, return error */
    if (ret != FILE_FOUND) {
      (void)fprintf(stderr, "%s: could not read '%s': ",
		    argv_program, inpath);
      perror("");
      exit(1);
    }
    path_p = inpath;
  }
  
  /* if tag-find is NULL we assume we are looking for a debug-value */
  if (tag_find == NULL) {
    
    /* now look for the value in the default token list */
    if (token != NULL) {
      for (def_p = defaults; def_p->de_string != NULL; def_p++) {
	if (def_p->de_flags == debug_value) {
	  (void)loc_snprintf(token, token_size, "internal token: %s",
			     def_p->de_string);
	  new_debug = def_p->de_flags;
	  break;
	}
      }
      if (def_p->de_string == NULL) {
	(void)loc_snprintf(token, token_size, "unknown token");
	new_debug = 0;
      }
    }
  }
  else {
    
    /* now look for the token in the default token list */
    for (def_p = defaults; def_p->de_string != NULL; def_p++) {
      if (strcmp(tag_find, def_p->de_string) == 0) {
	new_debug = def_p->de_flags;
	break;
      }
    }
    
    /* did we not find the token? */
    if (def_p->de_string == NULL) {
      if (path_p == NULL) {
	(void)fprintf(stderr, "%s: unknown tag '%s'\n",
		      argv_program, tag_find);
      }
      else {
	(void)fprintf(stderr, "%s: could not find tag '%s' in '%s'\n",
		      argv_program, tag_find, path_p);
      }
      exit(1);
    }
  }
  
  return new_debug;
}

/*
 * List the tags that in the files.
 */
static	void	list_tags(void)
{
  char		path[1024], *path_p, token[80];
  default_t	*def_p;
  const char	*home_p;
  long		new_debug = 0;
  FILE		*rc_file;
  
  /* do we need to have a home variable? */
  if (inpath == NULL) {
    
    /* first we try to read the RC file from the current directory */
    rc_file = fopen(DEFAULT_CONFIG, "r");
    if (rc_file == NULL) {
      
      /* if no file in current directory, try home directory */
      home_p = getenv(HOME_ENVIRON);
      if (home_p == NULL) {
	(void)fprintf(stderr, "%s: could not find variable '%s'\n",
		      argv_program, HOME_ENVIRON);
	exit(1);
      }
      
      (void)loc_snprintf(path, sizeof(path), "%s/%s", home_p, DEFAULT_CONFIG);
      path_p = path;
      
      rc_file = fopen(path, "r");
      /* we don't check for error right here */
    }
    else {
      path_p = DEFAULT_CONFIG;
    }
  }
  else {
    
    /* open the specified file */
    rc_file = fopen(inpath, "r");
    /* we assume that if the file was specified, it must be there */
    if (rc_file == NULL) {
      (void)fprintf(stderr, "%s: could not read '%s': ",
		    argv_program, inpath);
      perror("");
      exit(1);
    }
    path_p = inpath;
  }
  
  if (rc_file != NULL) {
    (void)fprintf(stderr, "Tags available from '%s':\n", path_p);
    
    while (read_next_token(rc_file, &new_debug, token, sizeof(token)) == 1) {
      if (verbose_b) {
	(void)fprintf(stderr, "%s (%#lx):\n", token, new_debug);
	dump_debug(new_debug);
      }
      else {
	(void)fprintf(stderr, "%s\n", token);
      }
    }
    
    (void)fclose(rc_file);
  }
  
  (void)fprintf(stderr, "\n");
  (void)fprintf(stderr, "Tags available by default:\n");
  
  for (def_p = defaults; def_p->de_string != NULL; def_p++) {
    if (verbose_b) {
      (void)fprintf(stderr, "%s (%#lx):\n",
		    def_p->de_string, def_p->de_flags);
      dump_debug(def_p->de_flags);
    }
    else {
      (void)fprintf(stderr, "%s\n", def_p->de_string);
    }
  }
}

/*
 * dump the current settings of the malloc variables
 */
static	void	dump_current(void)
{
  char		*log_path, *loc_start_file, token[64];
  const char	*env_str;
  DMALLOC_PNT	addr;
  unsigned long	inter, limit_val, loc_start_size, loc_start_iter;
  long		addr_count;
  int		lock_on, loc_start_line;
  unsigned int	flags;
  
  /* get the options flag */
  env_str = getenv(OPTIONS_ENVIRON);
  if (env_str == NULL) {
    env_str = "";
  }
  _dmalloc_environ_process(env_str, &addr, &addr_count, &flags,
			   &inter, &lock_on, &log_path,
			   &loc_start_file, &loc_start_line, &loc_start_iter,
			   &loc_start_size, &limit_val);
  
  if (flags == 0) {
    (void)fprintf(stderr, "Debug-Flags  not-set\n");
  }
  else {
    (void)find_tag(flags, NULL, token, sizeof(token));
    (void)fprintf(stderr, "Debug-Flags %#x (%u) (%s)\n",
		  flags, flags, token);
    if (verbose_b) {
      dump_debug(flags);
    }
  }
  
  if (addr == NULL) {
    (void)fprintf(stderr, "Address      not-set\n");
  }
  else {
    if (addr_count == 0) {
      (void)fprintf(stderr, "Address      %#lx\n", (long)addr);
    }
    else {
      (void)fprintf(stderr, "Address      %#lx, count = %lu\n",
		    (long)addr, addr_count);
    }
  }
  
  if (inter == 0) {
    (void)fprintf(stderr, "Interval     not-set\n");
  }
  else {
    (void)fprintf(stderr, "Interval     %lu\n", inter);
  }
  
  if (lock_on == 0) {
    (void)fprintf(stderr, "Lock-On      not-set\n");
  }
  else {
    (void)fprintf(stderr, "Lock-On      %d\n", lock_on);
  }
  
  if (log_path == NULL) {
    (void)fprintf(stderr, "Logpath      not-set\n");
  }
  else {
    (void)fprintf(stderr, "Logpath      '%s'\n", log_path);
  }
  
  if (limit_val == 0) {
    (void)fprintf(stderr, "Mem-Limit    not-set\n");
  }
  else {
    (void)fprintf(stderr, "Mem-Limit    %lu\n", limit_val);
  }
  
  if (loc_start_file != NULL) {
    (void)fprintf(stderr, "Start-File   '%s', line = %d\n",
		  loc_start_file, loc_start_line);
  }
  else if (loc_start_iter > 0) {
    (void)fprintf(stderr, "Start-Count  %lu\n", loc_start_iter);
  }
  else if (loc_start_size > 0) {
    (void)fprintf(stderr, "Start-Size   %lu\n", loc_start_size);
  }
  else {
    (void)fprintf(stderr, "Start        not-set\n");
  }
  
  (void)fprintf(stderr, "\n");
  (void)fprintf(stderr, "Debug Malloc Utility: http://dmalloc.com/\n");
  (void)fprintf(stderr,
		"  For a list of the command-line options enter: %s --usage\n",
		argv_argv[0]);
}

/*
 * output the code to set env VAR to VALUE
 */
static	void    set_variable(const char *var, const char *value)
{
  char	comm[1024];
  
  if (value == NULL || *value == '\0') {
    (void)loc_snprintf(comm, sizeof(comm), "unset %s\n", var);
  }
  else if (bourne_b) {
    (void)loc_snprintf(comm, sizeof(comm), "%s=%s\nexport %s\n",
		       var, value, var);
  }
  else if (rcshell_b) {
    (void)loc_snprintf(comm, sizeof(comm), "%s='%s'\n", var, value);
  }
  else if (gdb_b) {
    (void)loc_snprintf(comm, sizeof(comm), "set env %s %s\n", var, value);
  }
  else {
    (void)loc_snprintf(comm, sizeof(comm), "setenv %s %s\n", var, value);
  }
  
  if (make_changes_b) {
    (void)printf("%s", comm);
  }
  if ((! make_changes_b) || verbose_b) {
    (void)fprintf(stderr, "Outputed:\n");
    (void)fprintf(stderr, "%s", comm);
  }
}

/*
 * Returns the string for ERROR_NUM.
 */
static	char	*local_strerror(const int error_num)
{
  error_str_t	*err_p;
  
  for (err_p = error_list; err_p->es_error != 0; err_p++) {
    if (err_p->es_error == error_num) {
      return err_p->es_string;
    }
  }
  
  return INVALID_ERROR;
}

/*
 * static void header
 *
 * DESCRIPTION:
 *
 * Print out a little usage header to the user.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
static	void	header(void)
{
  (void)fprintf(stderr,
		"Debug Malloc Utility: http://dmalloc.com/\n");
  (void)fprintf(stderr,
		"  This utility helps set the Debug Malloc environment variables.\n");
}

int	main(int argc, char **argv)
{
  char		buf[1024];
  int		set_b = 0;
  char		*log_path, *loc_start_file;
  const char	*env_str;
  DMALLOC_PNT	addr;
  unsigned long	inter, limit_val, loc_start_size, loc_start_iter;
  unsigned long	addr_count;
  int		lock_on;
  int		loc_start_line;
  unsigned int	flags;
  
  argv_help_string = "Sets dmalloc library env variables.  Also try --usage.";
  argv_version_string = dmalloc_version;
  argv_usage_type = ARGV_USAGE_LONG;
  argv_error_type = ARGV_USAGE_SHORT_REM;
  
  argv_process(args, argc, argv);
  
  if (help_b) {
    header();
    (void)fprintf(stderr,
		  "  For a list of the command-line options enter: %s --usage\n",
		  argv_argv[0]);
    exit(0);
  }
  if (usage_b) {
    header();
    argv_usage(args, argv_usage_type);
    exit(0);
  }
  if (version_b) {
    (void)fprintf(stderr, "Dmalloc utility version string is: %s\n",
		  argv_version_string);
    (void)fprintf(stderr,
		  "  NOTE: Library linked with your application may be a different version.\n");
    (void)fprintf(stderr,
		  "        Check top of logfile after application is run for library version.\n");
    exit(0);
  }
  
  if (very_verbose_b) {
    verbose_b = 1;
  }
  
  /* try to figure out the shell we are using */
  if ((! bourne_b) && (! cshell_b) && (! gdb_b) && (! rcshell_b)) {
    choose_shell();
  }
  
  /* get the current debug information from the env variable */
  env_str = getenv(OPTIONS_ENVIRON);
  if (env_str == NULL) {
    env_str = "";
  }
  _dmalloc_environ_process(env_str, &addr, &addr_count, &flags, &inter,
			   &lock_on, &log_path, &loc_start_file,
			   &loc_start_line, &loc_start_iter, &loc_start_size,
			   &limit_val);
  
  /*
   * So, if a tag was specified on the command line then we set the
   * debug from it.  If it was not then we see if the debug flags were
   * set as a hex value from the -d.  If this was not used then take
   * the current value.
   */
  if (tag == NULL) {
    if (argv_was_used(args, DEBUG_ARG)) {
      set_b = 1;
      /* should we clear the rest? */
      if (remove_auto_b && (! keep_b)) {
	clear_b = 1;
      }
    }
    else {
      debug = flags;
    }
  }
  else {
    if (argv_was_used(args, DEBUG_ARG)) {
      (void)fprintf(stderr, "%s: warning -d ignored, processing tag '%s'\n",
		    argv_program, tag);
    }
    set_b = 1;
    debug = find_tag(0L, tag, NULL, 0);
    /* should we clear the rest? */
    if (remove_auto_b && (! keep_b)) {
      clear_b = 1;
    }
  }
  
  if (plus.aa_entry_n > 0) {
    int		plus_c;
    for (plus_c = 0; plus_c < plus.aa_entry_n; plus_c++) {
      BIT_SET(debug, token_to_value(ARGV_ARRAY_ENTRY(plus, char *, plus_c)));
      set_b = 1;
    }
  }
  
  if (minus.aa_entry_n > 0) {
    int		minus_c;
    for (minus_c = 0; minus_c < minus.aa_entry_n; minus_c++) {
      BIT_CLEAR(debug,
		token_to_value(ARGV_ARRAY_ENTRY(minus, char *, minus_c)));
      set_b = 1;
    }
  }
  
  if (address != NULL) {
    _dmalloc_address_break(address, &addr, &addr_count);
    set_b = 1;
  }
  else if (clear_b) {
    addr = NULL;
  }
  
  if (argv_was_used(args, INTERVAL_ARG)) {
    inter = interval;
    set_b = 1;
  }
  else if (clear_b) {
    inter = 0;
  }
  
  /*
   * NOTE: this should be after the debug setting which this tests.
   */
  if (argv_was_used(args, THREAD_LOCK_ON_ARG)) {
    lock_on = thread_lock_on;
    set_b = 1;
  }
  else if (clear_b) {
    lock_on = 0;
  }
  
  if (logpath != NULL) {
    log_path = logpath;
    set_b = 1;
  }
  else if (clear_b) {
    log_path = NULL;
  }
  
  if (start_file != NULL) {
    _dmalloc_start_break(start_file, &loc_start_file, &loc_start_line,
			 &loc_start_iter, &loc_start_size);
    set_b = 1;
  }
  else if (start_iter > 0) {
    loc_start_file = NULL;
    loc_start_line = 0;
    loc_start_iter = start_iter;
    loc_start_size = 0;
    set_b = 1;
  }
  else if (start_size > 0) {
    loc_start_file = NULL;
    loc_start_line = 0;
    loc_start_iter = 0;
    loc_start_size = start_size;
    set_b = 1;
  }
  else if (clear_b) {
    loc_start_file = NULL;
    loc_start_line = 0;
    loc_start_iter = 0;
    loc_start_size = 0;
  }
  
  if (argv_was_used(args, LIMIT_ARG)) {
    limit_val = limit_arg;
    set_b = 1;
  }
  
  if (errno_to_print > 0) {
    (void)fprintf(stderr, "%s: dmalloc_errno value '%d' = \n",
		  argv_program, errno_to_print);
    (void)fprintf(stderr, "   '%s'\n", local_strerror(errno_to_print));
  }
  
  if (list_tags_b) {
    list_tags();
  }
  
  if (debug_tokens_b) {
    attr_t		*attr_p;
    unsigned int	left = 0x7fffffff;
    
    (void)fprintf(stderr, "Debug Tokens:\n");
    for (attr_p = attributes; attr_p->at_string != NULL; attr_p++) {
      /* skip any disabled tokens */
      if (attr_p->at_value == 0 && strcmp(attr_p->at_string, "none") != 0) {
	continue;
      }
      if (attr_p->at_value != 0 && (! BIT_IS_SET(left, attr_p->at_value))) {
	/* skip any tokens we've seen before */
	continue;
      }
      if (very_verbose_b) {
	(void)fprintf(stderr, "%s -- %s (%#lx)\n",
		      attr_p->at_string, attr_p->at_desc, attr_p->at_value);
      }
      else if (verbose_b) {
	(void)fprintf(stderr, "%s -- %s\n",
		      attr_p->at_string, attr_p->at_desc);
      }
      else {
	(void)fprintf(stderr, "%s\n", attr_p->at_string);
      }
      BIT_CLEAR(left, attr_p->at_value);
    }
  }
  
  if (clear_b || set_b) {
    _dmalloc_environ_set(buf, sizeof(buf), long_tokens_b, addr, addr_count,
			 debug, inter, lock_on, log_path, loc_start_file,
			 loc_start_line, loc_start_iter, loc_start_size,
			 limit_val);
    set_variable(OPTIONS_ENVIRON, buf);
  }
  else if (errno_to_print == 0
	   && (! list_tags_b)
	   && (! debug_tokens_b)) {
    dump_current();
  }
  
  argv_cleanup(args);
  
  exit(0);
}
