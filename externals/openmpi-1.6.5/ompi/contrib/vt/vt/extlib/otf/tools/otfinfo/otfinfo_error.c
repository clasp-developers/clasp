/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Michael Heyde
*/

#include "otfinfo_error.h"

#include <stdio.h>
#include <stdlib.h>

void otfinfo_assert_impl(const char* file, int line, const char* expr)
{
  fprintf(stderr, "FATAL: %s:%d: Assertion `%s' failed\n",file, line, expr);
  exit(1);
}
