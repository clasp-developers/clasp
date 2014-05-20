/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * expand.h - expand a buffer, given variable values
 */

#include "lists.h"

LIST *var_expand( LIST  *l, char *in, char *end, LOL *lol, int cancopyin );
void var_expand_unit_test();
