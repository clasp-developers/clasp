/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * option.h - command line option processing
 *
 * {o >o
 *  \ -) "Command line option."
 */

typedef struct option
{
    char    flag;       /* filled in by getoption() */
    char    *val;       /* set to random address if true */
} option;

# define N_OPTS 256

int     getoptions( int argc, char **argv, char *opts, option *optv );
char *  getoptval( option *optv, char opt, int subopt );
